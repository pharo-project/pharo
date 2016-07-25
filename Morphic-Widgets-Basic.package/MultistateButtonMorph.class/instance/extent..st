geometry
extent: aPoint
	"Center the fill style origin."
	
	|delta|
	self bounds extent = aPoint ifTrue: [^self].
	delta := aPoint - self extent // 2.
	self fillStyles do: [:fs | fs isOrientedFill ifTrue: [fs origin: fs origin + delta]].
	super extent: aPoint