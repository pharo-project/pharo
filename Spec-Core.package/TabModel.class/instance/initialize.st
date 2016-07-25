initialization
initialize
	"Initialization code for TabModel"

	super initialize.

	labelHolder := '' asValueHolder.
	iconHolder := self defaultIcon asValueHolder.
	morphHolder := nil asValueHolder.
	retrievingBlockHolder := nil asValueHolder.
	menuHolder := nil asValueHolder.
	actionsHolder := OrderedCollection new asValueHolder.
	closeableHolder := true asValueHolder.
	tabSelectedActionHolder := [  ] asValueHolder.
	selectedHolder := false asValueHolder