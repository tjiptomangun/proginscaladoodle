class SlowAppendQueue[T](elems: List[T]){
	def head = elems.head
	def tail = new SlowAppendQueue(elems.tail)
	def enqueue(x: T) = new SlowAppendQueue(elems ::: List(x))
}

class SlowHeadQueue[T](smele: List[T]){
	def head = smele.last
	def tail = new SlowHeadQueue(smele.init)
	def enqueue (x: T) = new SlowHeadQueue(x :: smele)
}
