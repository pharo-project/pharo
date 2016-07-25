private
shortcutClass
	"Answers first class that can be used. 
	 WARNING: Note than for speed I'm using #subclasses instead #allSubclasses, avoiding the scan of 
	 the hierarchy. This will not work if we extend the hierarchy more than one level"
	^ (self subclasses sorted: [ :a :b | a priority < b priority ])
		detect: #canBeUsed