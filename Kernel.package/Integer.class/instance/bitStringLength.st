bit manipulation
bitStringLength

      ^self digitLength * 8
           	"make sure positive integer bitString always begins with 0"
           + (self positive ifTrue: [1] ifFalse: [0])