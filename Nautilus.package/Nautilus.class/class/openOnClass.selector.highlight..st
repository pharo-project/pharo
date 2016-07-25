opening
openOnClass: class selector: selector highlight: autoSelectString
	| nautilus |

	nautilus := self openOnClass: class selector: selector.
	autoSelectString ifNotNil: [ nautilus ui highlight: autoSelectString ]