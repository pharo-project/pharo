browse-nautilus
browseInOtherNautilus
	| classToBrowse |
	classToBrowse := self content reference.
	self browseClass: classToBrowse