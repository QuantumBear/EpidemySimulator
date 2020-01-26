package simulator

import scala.collection.mutable.ListBuffer

class Simulator {
  type Action = () => Unit

  protected type Agenda = ListBuffer[WorkItem]

  case class WorkItem(time: Int, action: Action)

  protected val agenda:Agenda = ListBuffer()
  protected var currentTime = 0

  protected def afterDelay(delay: Int)(action: => Unit) {
    val item = WorkItem(currentTime + delay, () => action)
    if (agenda.isEmpty) {
      agenda.append(item)
    } else {
      var pos = agenda.indexWhere(_.time > item.time)
      if (pos == -1) agenda.append(item)
      else agenda.insert(pos, item)
    }
  }

  protected def next {
    if (agenda.isEmpty) {}
    else {
      val item = agenda.head
      agenda.remove(0)
      currentTime = item.time
      item.action()
    }
  }

  def run {
    println("*** New propagation ***")
    while (!agenda.isEmpty) { next }
  }
}
