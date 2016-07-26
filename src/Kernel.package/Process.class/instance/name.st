accessing
name
 
 	^name ifNil: [ self hash asString forceTo: 10 paddingStartWith: $ ]