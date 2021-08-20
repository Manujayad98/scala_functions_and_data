object Q4 extends App {
  var bank:List[BankAccs]=List(new BankAccs("Nimal",314,-65000),
                                new BankAccs("Kamal",324,-100000),
                                new BankAccs("Piyal",325,-25000),
                                new BankAccs("Sunil",327,125000),
                                new BankAccs("Kasun",375,85000)) 
  val findOd=(l:List[BankAccs])=>l.filter(x=>x.b<0)
  val finalbal=(l:List[BankAccs])=>l.map(x=>if (x.b< 0) new BankAccs(x.n,x.a,x.b*1.001) else new BankAccs(x.n,x.a,x.b*1.0005))
  val getsum=(l:List[BankAccs])=> l.reduce((x,y)=>(new BankAccs("",0,x.b+y.b)))
  println("OD accounts : ")
  findOd(bank).foreach(k=>println("Acc No : "+k.a+" Name :"+k.n +" OD :"+k.b+" LKR"))
  println("\nSum of all accounts : ")
  println(getsum(bank).b+" LKR")
  println("\nFinal balances : ")
  finalbal(bank).foreach(k=>println("Acc No : "+k.a+" Name :"+k.n +" Final Balance :" + k.b+" LKR"))
  
  
}
class BankAccs(name:String,accno:Int,bal:Double){
  var n=name
  var a=accno
  var b=bal
}
  


  
