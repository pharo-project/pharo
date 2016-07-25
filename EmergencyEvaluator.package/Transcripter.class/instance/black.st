private
black
	Display depth = 1 ifTrue: [^ Bitmap with: 16rFFFFFFFF "Works without color support"].
	^ Color black