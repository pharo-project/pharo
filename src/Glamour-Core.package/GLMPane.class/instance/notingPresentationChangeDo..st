private
notingPresentationChangeDo: aBlock
	"Executes aBlock. If during the execution of the block one of my presentations change, I will emit announces. I also take care of not entering an infinite loop."
	| oldPresentations oldMatchingPresentations | 
	self isAllowedToNotePresentationsChanged ifFalse: [
		aBlock value.
		^ self].
	"this should create a context and then check for it before announcing.
	Like this we can deal with the problem of announcing the same announcement twice"
	allowedToNotePresentationsChanged := false.
	
	oldPresentations := presentations presentations copy. 
	oldMatchingPresentations := self cachedMatchingPresentations.
	aBlock value. 
	presentations presentations  ~= oldPresentations ifTrue: [
		self announce: ((GLMPresentationsChanged new) 
						pane: self; 
						oldPresentations: oldPresentations; 
						yourself)]. 
	self matchingPresentations ~= oldMatchingPresentations ifTrue: [
		self announce: ((GLMMatchingPresentationsChanged new) 
						pane: self; 
						oldMatchingPresentations: oldMatchingPresentations; 
						yourself)].
	allowedToNotePresentationsChanged := true.
