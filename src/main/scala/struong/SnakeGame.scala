package struong

import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

// https://blog.rockthejvm.com/snake/
object SnakeGame extends JFXApp3 {

  val initialSnake: List[(Double, Double)] = List(
    (250, 200),
    (225, 200),
    (200, 200)
  )

  def gameLoop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep((1000 / 25) * 2) // 1000 / 25 = 25 fps
    }.flatMap(_ => Future(gameLoop(update)))

  case class State(
      snake: List[(Double, Double)],
      food: (Double, Double)
  ) {
    def newState(direction: Int): State = {
      val (x, y) = snake.head
      val (newx, newy) = direction match {
        case 1 => (x, y - 25)
        case 2 => (x, y + 25)
        case 3 => (x - 25, y)
        case 4 => (x + 25, y)
        case _ => (x, y)
      }

      val newSnake: List[(Double, Double)] = {
        val isOutOfBounds = x < 0 || x >= 600 || newy < 0 || newy >= 600
        val hasHitTail = snake.tail.contains((newx, newy))

        if (isOutOfBounds || hasHitTail) // crashed
          initialSnake
        else if (food == (newx, newy)) // eaten food
          food :: snake
        else
          (newx, newy) :: snake.init // carry on
      }

      val newFood = if (food == (newx, newy)) randomFood() else food

      State(newSnake, newFood)
    }

    def rectangles: List[Rectangle] = rect(food._1, food._2, Red) :: snake.map {
      case (x, y) => rect(x, y, Green)
    }
  }

  def randomFood(): (Double, Double) = {
    val next = 600 / 25
    (Random.nextInt(next) * 25, Random.nextInt(next) * 25)
  }

  def rect(xr: Double, yr: Double, colour: Color): Rectangle = new Rectangle {
    x = xr
    y = yr
    width = 25
    height = 25
    fill = colour
  }

  override def start(): Unit = {
    val state = ObjectProperty(State(initialSnake, randomFood()))
    val frame = IntegerProperty(0)
    val direction = IntegerProperty(4) // right

    frame.onChange {
      state.update(state.value.newState(direction.value))
    }

    stage = new JFXApp3.PrimaryStage {
      title = "Snake"
      width = 600
      height = 600
      scene = new Scene {
        fill = White
        content = state.value.rectangles
        onKeyPressed = key =>
          key.getText match {
            case "w" => direction.value = 1
            case "s" => direction.value = 2
            case "a" => direction.value = 3
            case "d" => direction.value = 4
            case _   => direction.value = direction.value
          }
        state.onChange {
          Platform.runLater {
            content = state.value.rectangles
          }
        }
      }
    }

    gameLoop(() => frame.update(frame.value + 1))
  }
}
