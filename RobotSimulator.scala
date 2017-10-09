object Bearing extends Enumeration {
  val North, South, East, West = Value
}

case class Robot(var bearing: Bearing.Value, var coordinates: (Int, Int)) {

  def turnRight(): Robot = {
    bearing = 
      bearing match {
        case Bearing.North => Bearing.East
        case Bearing.East => Bearing.South
        case Bearing.South => Bearing.West
        case Bearing.West => Bearing.North  
      }
    Robot(bearing, coordinates)
  }

  def turnLeft(): Robot = {
    bearing = 
      bearing match {
        case Bearing.North => Bearing.West
        case Bearing.East => Bearing.North
        case Bearing.South => Bearing.East
        case Bearing.West => Bearing.South  
      }
    Robot(bearing, coordinates)
  }

  def advance(): Robot = {
    val (x, y) = coordinates
    coordinates = 
      bearing match {
        case Bearing.North => (x, y + 1)
        case Bearing.East => (x + 1, y)
        case Bearing.South => (x, y - 1)
        case Bearing.West => (x - 1, y)
      }
    Robot(bearing, coordinates)
  }

  def simulate(s: String): Robot = {
    s.toList.foreach((x) => {
      if (x == 'R'){
        bearing = 
          bearing match {
            case Bearing.North => Bearing.East
            case Bearing.East => Bearing.South
            case Bearing.South => Bearing.West
            case Bearing.West => Bearing.North  
          }
      }
      else if (x == 'L'){
        bearing = 
          bearing match {
            case Bearing.North => Bearing.West
            case Bearing.East => Bearing.North
            case Bearing.South => Bearing.East
            case Bearing.West => Bearing.South  
          }
      }
      else{
        val (x, y) = coordinates
        coordinates = 
          bearing match {
            case Bearing.North => (x, y + 1)
            case Bearing.East => (x + 1, y)
            case Bearing.South => (x, y - 1)
            case Bearing.West => (x - 1, y)
          }
      }
    })
    Robot(bearing, coordinates)
  }
}