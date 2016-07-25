private
collectArguments: anArgArray
	"Private"

    | staticArgs |
    staticArgs := self arguments.
    ^(anArgArray size = staticArgs size)
        ifTrue: [anArgArray]
        ifFalse:
            [(staticArgs isEmpty
                ifTrue: [ staticArgs := Array new: selector numArgs]
                ifFalse: [staticArgs copy] )
                    replaceFrom: 1
                    to: (anArgArray size min: staticArgs size)
                    with: anArgArray
                    startingAt: 1]