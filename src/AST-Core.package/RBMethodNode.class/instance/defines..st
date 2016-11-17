testing
defines: aName
^ (arguments anySatisfy: [ :each | each name = aName ])

		or: [ self pragmas anySatisfy: [ :pragma | pragma defines: aName ] ]