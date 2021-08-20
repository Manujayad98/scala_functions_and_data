object Q2 extends App {
  val x=new Rational(3,4)
  val y=new Rational(5,8)
  val z=new Rational(2,7)

  println(x-y-z)
  
}
class Rational(n:Int,d:Int){
  //to simplify
  def gcd(a:Int,b:Int):Int=
    if(b==0) a else gcd(b,a%b)
  val s=gcd(n,d)
  
  def numer=n/s
  def denom=d/s

  //to get negative
  def neg=new Rational(-this.numer,this.denom)

  //add
  def +(r:Rational)=new Rational(r.denom*numer+r.numer*denom,denom*r.denom)
  
  def -(r:Rational)=this+r.neg
  
  override def toString= numer+"/"+ denom
}
