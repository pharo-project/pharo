accessing
selectedIndex
	"Answer the value of selectedIndex"

	^ selectedIndex ifNil: [ selectedIndex := 0 ]