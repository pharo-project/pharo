I provide a simple way to test the hash properties of any object.  

I am given an object that should be tested and I treat it like a prototype.  I take a copy of it when I am given it so that it can't change whilst I am holding on to it.  I can then test that multiple copies of this object all hash to the same value.