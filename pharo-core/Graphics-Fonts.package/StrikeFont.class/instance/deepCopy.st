copying
deepCopy
 " there is a circular reference from the derivative fonts back to the receiver. It is therefore not possible to make a deep copy. We make a sahllow copy. The method postCopy can be used to modify the shallow copy. " 
  ^self copy