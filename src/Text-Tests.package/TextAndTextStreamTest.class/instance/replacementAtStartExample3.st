examples
replacementAtStartExample3

   | text1  replacement  length  |

   text1 := 'This is a simple text' copy asText.
    " without the copy, we would modify a constant that the compiler attached at the compiled method. "
   length  := 'This' size.
   replacement := 'Tht' asText.
   text1 replaceFrom: 1 
        to:   length
        with: replacement
        startingAt: 1.
