// This file is interpreted, it will not load
// as a Scala package. To run, start scala from
// the command line and type :
// 
//     scala> :load monad.scala
// 

case class Order()

trait MaybeOrder {
    def map ( process: Order => Order )      : MaybeOrder
    def flatMap ( process: Order => MaybeOrder ) : MaybeOrder
}

case class GoodOrder( order: Order ) extends MaybeOrder {
    def map ( process: Order => Order ) = {
        GoodOrder( process(order) )
    }
    def flatMap ( process: Order => MaybeOrder ) = {
        process(order)
    }
}
case class BadOrder() extends MaybeOrder {
    def map ( process: Order => Order ) = {
        BadOrder()
    }
    def flatMap ( process: Order => MaybeOrder ) = {
        BadOrder()
    }
}

val goodOrder = GoodOrder( new Order() )
val badOrder  = BadOrder()

def creditCheck ( order: Order ) : MaybeOrder = {
    GoodOrder(order)
}

def stockCheck  ( order: Order ) : MaybeOrder = {
    GoodOrder(order)
}

def process ( order: Order ) : Order = {
    println("Order Successfully Placed")
    order
}

def save( order: Order ) : Order = {
    println("Order Saved to System")
    order
}

goodOrder flatMap { creditCheck } flatMap { stockCheck } map process map save
badOrder  flatMap { creditCheck } flatMap { stockCheck } map process map save


def addressValid  ( order: Order ) : MaybeOrder = {
    GoodOrder(order)
}

goodOrder flatMap { creditCheck } flatMap { stockCheck } flatMap { addressValid } map process map save

for ( o1 <- goodOrder;
      o2 <- creditCheck(o1);
      o3 <- stockCheck(o2);
      o4 <- addressValid(o3)
    ) yield process(o4)




