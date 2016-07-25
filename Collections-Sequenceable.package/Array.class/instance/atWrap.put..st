accessing
atWrap: index put: anObject
	"Optimized to go through the primitive if possible"
	<primitive: 61>
	^ self at: index - 1 \\ self size + 1 put: anObject