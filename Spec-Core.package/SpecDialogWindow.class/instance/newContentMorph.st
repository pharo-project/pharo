accessing
newContentMorph
	"Answer a new content morph."

	^ self specWidget 
			ifNil: [ nil ] 
			ifNotNil: [:w | 
				w
					hResizing: #spaceFill;
					vResizing: #spaceFill ]