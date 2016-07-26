private helpers
allSettingsState
	"
	Workspace openContents: self allSettingsState label: 'Current Setting assignments' 
	"
	| declList |
	declList := (self currentNodeList select: [ :node | node item hasValue])
		collect: [ :node | node item ].
	^ String streamContents: [:s | declList do: [:d | d settingStoreOn: s] ]