accessing
hasToggleAtRoot
	^ hasToggleAtRoot ifNil: [hasToggleAtRoot := self roots anySatisfy: [:s | s hasToggle]]
