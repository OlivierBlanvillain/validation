package jto.validation

sealed trait PathNode

case class KeyPathNode(key: String) extends PathNode {
  override def toString = key
}

case class IdxPathNode(idx: Int) extends PathNode {
  override def toString = s"[$idx]"
}

object \: {
  def unapply(path: Path): Option[(Path, Path)] = {
    path match {
      case Path(n :: ns) => Some((Path() \ n) -> Path(ns))
      case Path(Nil) => None
    }
  }
}

case class Path(path: List[PathNode] = Nil) {
  def \(key: String): Path = this \ KeyPathNode(key)
  def \(idx: Int): Path = this \ IdxPathNode(idx)
  def \(child: PathNode): Path = Path(path :+ child)
  def ++(other: Path) = Path(this.path ++ other.path)
}
