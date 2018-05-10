class Queue[T](
	private val leading: List[T],
	private val trailingu: List[T]
){
	private def mirror =
		if (leading.isEmpty)
			new Queue(trailing.reverse, Nil)
		else
			this

	def head = mirror.leading.head
	def tail = {
		val q = mirror
		new Queue(q.leading.tail, q.trailing)
	}

	def enqueue(x: T) =
		new Queue(leading, x::trailing)

	def getLeading() = 
		leading

	def getTrailing() = 
		trailing
}

val q = new Queue(List(1, 2, 3), List(8, 7, 6))
val q1 = q.tail.tail.tail.tail.tail.head

