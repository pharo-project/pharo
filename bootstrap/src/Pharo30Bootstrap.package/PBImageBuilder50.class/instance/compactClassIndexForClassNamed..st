helpers
compactClassIndexForClassNamed: aName
	"To recompute, use the following expression in the image you want to bootstrap. For example, to boostrap pharo 50193, execute the expression in a pharo 50193 image.
	(Smalltalk specialObjectsArray at: 29)
		collect: [ :each | each ifNotNil: [ each name ] ]
	The order is really important, do not change it!
		 "
	^ #(
		#CompiledMethod
		#Slot
		#Array
		#LargeNegativeInteger
		#LargePositiveInteger
		#Float
		#Protocol
		#Association
		#Point
		#Rectangle
		#ByteString
		#BlockClosure
		#GlobalVariable
		#Context
		#ClassVariable
		#Bitmap
		#EndianDetector
		nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
	 indexOf: aName