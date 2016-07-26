actions
browseClass
	"Browse the class of the selected item."

	^ Smalltalk tools browser
		openOnClass: self selectedChangeWrapper actualClass
		selector: self selectedMessageName