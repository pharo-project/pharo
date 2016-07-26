accessing
lines
    "Answer an array of lines composing this receiver without the line ending delimiters"
    ^Array new: (self size // 60 max: 16)
            streamContents: [:lines | self linesDo: [:aLine | lines nextPut: aLine]]