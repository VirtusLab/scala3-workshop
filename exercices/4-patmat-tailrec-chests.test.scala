//> using toolkit default

package ex4

sealed trait Item { def id: Item.Id }
object Item {
  type Id = Int
  case class Chest(id: Id, items: List[Item]) extends Item
  case class Key(id: Id) extends Item
}
import Item._

// Each chest has at most 1 matching key
// Traverse the sequence of items and return ids of all the chests that can be opened
// When you find the key you can add it to your storage
// When you find the chest you MUST use open the chest with a key or forget about it
// When opening the chest you need to take actions on its content before continuing travesal (depth-first search)
// Do not use any intermiediete val or var (you can use function arguments)
def listAccessibleChests(queue: List[Item]): Set[Item.Id] =
  def loop(
      queue: List[Item],
      opened: Set[Item.Id],
      storage: Map[Item.Id, Key]
  ): Set[Item.Id] =
    queue match {
      case Nil => opened
      case (key @ Key(id)) :: tail =>
        loop(queue = tail, opened = opened, storage = storage.updated(id, key))
      case Chest(id, items) :: tail =>
        storage.get(id) match {
          case None =>
            loop(queue = tail, opened = opened, storage = storage.removed(id))
          case Some(_) =>
            loop(
              queue = items ::: tail,
              opened = opened + id,
              storage = storage.removed(id)
            )
        }
    }
  loop(queue = queue, opened = Set.empty, storage = Map.empty)
end listAccessibleChests

class PatMatChestsSuite extends munit.FunSuite {
  import Item._
  test("Returns nil if cannot open any chests") {
    assertEquals(Set.empty, listAccessibleChests(Nil))
    assertEquals(Set.empty, listAccessibleChests(List(Key(1))))
    assertEquals(Set.empty, listAccessibleChests(List(Chest(1, Nil))))
  }

  test("Returns opened chests ids") {
    assertEquals(
      listAccessibleChests(Key(1) :: Chest(1, Nil) :: Nil),
      Set(1),
      "case 1"
    )

    assertEquals(
      listAccessibleChests(
        Key(1) ::
          Chest(2, items = Nil) ::
          Chest(1, items = Nil) ::
          Nil
      ),
      Set(1),
      "case 2"
    )

    assertEquals(
      {
        lazy val chest1 = Chest(1, items = Key(2) :: Nil)
        lazy val chest2 = Chest(2, items = Key(3) :: Key(4) :: chest3 :: Nil)
        lazy val chest3 = Chest(3, items = chest4 :: Key(5) :: Nil)
        lazy val chest4 = Chest(4, items = Chest(5, Nil) :: Nil)
        listAccessibleChests(
          List(
            Key(1),
            chest1,
            chest2
          )
        )
      },
      Set(1, 2, 3, 4)
    )
  }
}
