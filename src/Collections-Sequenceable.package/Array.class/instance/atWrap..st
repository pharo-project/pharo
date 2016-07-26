accessing
atWrap: index 
	"Optimized to go through the primitive if possible"
	<primitive: 60>
	^ self at: index - 1 \\ self size + 1