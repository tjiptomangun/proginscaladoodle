trait CovariantQueue[+T] {
	def head: T
	def tail: CovariantQueue[T]
	def enqueue[U >: T](x: U): CovariantQueue[U]
}


object CovariantQueue {
	def apply[T](xs: T*): CovariantQueue[T] =
		new CovariantQueueImpl[T](xs.toList, Nil)

	private class CovariantQueueImpl[T] (
		private val leading: List[T],
		private val trailing: List[T]
	) extends CovariantQueue[T] {
		def mirror =
			if (leading.isEmpty)
				new CovariantQueueImpl(trailing.reverse, Nil)
			else
				this

		def head: T = mirror.leading.head


		def tail: CovariantQueueImpl[T] = {
			val q = mirror
			new CovariantQueueImpl(q.leading.tail, q.trailing)
		}

		def enqueue[U >: T](x: U) =
			new CovariantQueueImpl(leading, x :: trailing)
	}
}

val q1 = CovariantQueue(List(1, 2, 3, 4, 5): _ *)
val q2 = q1.tail.tail.tail.tail.head
val q3 = q1.enqueue(6).enqueue(7)
val q4 = q3.tail.tail.tail.tail.tail.tail.head

case class fowl (fur: String, sex: String, habitate: String)
class duck (fur: String, sex: String) extends fowl(fur, sex, "water")
class chicken(fur: String, sex: String) extends fowl(fur, sex, "land")
class bird(fur: String, sex: String) extends fowl(fur, sex, "air")
class pigeon(fur: String, sex: String) extends bird(fur, sex)

val red = "red"
val green = "green"
val blue = "blue"
val male = "male"
val female = "female"
val air = "air"
val water = "water"
val land = "land"


val fowls = CovariantQueue(List(new fowl(red, male, land), new duck("brown", "female")): _*)
val fowls2 = fowls.enqueue(new pigeon("white", "male"))

trait OutputChannel[-T] {
	def  write(x: T)
}

object OutputChannelString extends OutputChannel[String]{
	def write(x: String) =
		println(x)
}

object OutputChannelAnyRef extends OutputChannel[AnyRef]{
	def write(x: AnyRef) =
		println(x)
}

val a: String = "Hello World"
OutputChannelAnyRef.write(a)

val b: AnyRef = List(1, 10)
//OutputChannelString.write(b) --> error
