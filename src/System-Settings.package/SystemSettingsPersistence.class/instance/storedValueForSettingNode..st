loading
storedValueForSettingNode: aSettingNode
	"It returns a stored value for the setting node.
	It returns nil value when there is no stored value."
	^ (self storedSettingForSettingNode: aSettingNode) ifNotNil: #realValue