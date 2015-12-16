import todo.{Context, Project, TodoParser}
import org.scalatest.{FlatSpec, Matchers}

class TaskTest extends FlatSpec with Matchers {

  it should "parse a Task with a priority" in {
    val task = parse("(A) Call mom")
    task.isSuccess should be(true)
    task.get.priority should be(Some("A"))
    task.get.description should be(Some("Call mom"))
  }

  it should "parse multiple tasks" in {
    val tasks = mParse("(A) Call Mom\n(B) call Dadd")
    tasks.isSuccess should be (true)
    tasks.get.size should be(2)
    tasks.get.foreach(print)
    tasks.get(0).priority should be(Some("A"))
    tasks.get(1).priority should be(Some("B"))
  }

  it should "parse tasks and when a priority exists, it ALWAYS appears first" in {
    val tasks = mParse("Really gotta call Mom (A) @phone @someday\n    (b) Get back to the boss\n    (B)->Submit TPS report")
    tasks.isSuccess should be(true)
    tasks.get.size should be(3)
    tasks.get.count(_.priority.isEmpty) should be(3)
  }

  it should "parse a task and ignore the priority when not '([A-Z]) '" in {
    val task = parse("(A)Call mom")
    task.isSuccess should be(true)
    task.get.priority should be(None)
    task.get.description should be(Some("(A)Call mom"))
  }

  it should "A task’s creation date may optionally appear directly after priority and a space" in {
    val tasks = mParse("2011-03-02 Document +TodoTxt task format\n    (A) 2011-03-02 Call Mom")
    tasks.isSuccess should be(true)
    tasks.get.size should be(2)
    tasks.get.count(!_.created.isEmpty) should be(2)
    tasks.get(0).created should be(Some("2011-03-02"))
    tasks.get(0).created should be(Some("2011-03-02"))

  }

  it should "not set the created date when not a start or after priority followed by space" in {
    val tasks = mParse("(A) Call Mom 2011-03-02\n  x 2011-03-02 not with date")
    tasks.isSuccess should be(true)
    tasks.get.count(_.created.isEmpty) should be(2)
    tasks.get(0).created should be(Some("2011-03-02"))
    tasks.get(1).created should be(Some("2011-03-02"))
  }

  it should "parse contexts and projects correctly" in {
    val task = parse("(A) Call Mom +Family +PeaceLoveAndHappiness @iphone @phone")
    task.isSuccess should be(true)
    task.get.priority should be(Some("A"))
    task.get.projects.contains(Project("Family"))  should be(true)
    task.get.projects.contains(Project("PeaceLoveAndHappiness"))  should be(true)

    task.get.contexts.contains(Context("iphone"))  should be(true)
    task.get.contexts.contains(Context("phone"))  should be(true)

  }

  it should "not find any contexts in these tasks" in {
    val tasks = mParse("Email SoAndSo at soandso@example.com\n Learn how to add 2+2")
    tasks.isSuccess should be(true)
    tasks.get.count(_.contexts.nonEmpty) should be(0)
  }

  it should "parse a completed task correctly" in {
    val task = parse("x 2011-03-03 Call Mom")
    task.isSuccess should be(true)
    task.get.completed should be(Some("2011-03-03"))
  }

  it should "not mark these tasks as complete" in {
    val tasks = mParse("xylophone lesson\n    X 2012-01-01 Make resolutions\n    (A) x Find ticket prices")
    tasks.get.count(_.completed.nonEmpty) should be(0)
  }

  it should "have completion date directly after the 'x'" in {
    val task = parse("x 2011-03-02 2011-03-01 Review Tim's pull request +TodoTxtTouch @github")
    task.isSuccess should be(true)
    task.get.completed should be(Some("2011-03-02"))
    task.get.created should be(Some("2011-03-01"))
  }


  private def parse(input: String) = {
    val parser = TodoParser(input)
    parser.task.run()
  }

  private def mParse(input: String) = {
    val parser = TodoParser(input)
    parser.tasks.run()
  }

}
