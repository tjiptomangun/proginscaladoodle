trait Queue[T] {
	def head: T
	def tail: Queue[T]
	def enqueue(x: T): Queue[T]
}


object Queue {
	def apply[T](xs: T*): Queue[T] =
		new QueueImpl[T](xs.toList, Nil)

	private class QueueImpl[T] (
		private val leading: List[T],
		private val trailing: List[T]
	) extends Queue[T] {
		def mirror =
			if (leading.isEmpty)
				new QueueImpl(trailing.reverse, Nil)
			else
				this

		def head: T = mirror.leading.head


		def tail: QueueImpl[T] = {
			val q = mirror
			new QueueImpl(q.leading.tail, q.trailing)
		}

		def enqueue(x: T) =
			new QueueImpl(leading, x :: trailing)
	}
}

val q1 = Queue(List(1, 2, 3, 4, 5): _ *)
val q2 = q1.tail.tail.tail.tail.head
val q3 = q1.enqueue(6).enqueue(7)
val q4 = q3.tail.tail.tail.tail.tail.tail.head


class StrangeIntQueue extends Queue[Int] {
	override def enqueue(x: Int) = {
		println(math.sqrt(x))
		super.enqueue(x)
	}
}

//val x: Queue[Any] = new StrangeIntQueue 

