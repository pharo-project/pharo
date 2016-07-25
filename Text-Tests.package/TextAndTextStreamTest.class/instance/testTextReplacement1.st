tests
testTextReplacement1

	"for a Text  t,
     the following assertion should always hold:
     t string size = t run size 
	This test examines the preservation of this assertion for in-place replacement "

   | text1 string replacement startPos length startPosInRep string2 |
   text1 := (string := 'This is a simple text' copy) asText.
   "without the copy, we would modify a constant that the compiler attached at the compiled method. "
   startPos := string findString: 'simple'. 
   length  := 'simple' size.
   replacement := (string2 := 'both simple and short*') asText.
   startPosInRep :=  string2 findString: 'short'.
   text1 replaceFrom: startPos 
        to: startPos + length - 1
        with: replacement
        startingAt: startPosInRep.
   self assert: text1 string size = text1 runs size.
