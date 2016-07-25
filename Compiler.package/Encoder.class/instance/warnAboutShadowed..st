private
warnAboutShadowed: name
	requestor addWarning: name,' is shadowed'.
	selector ifNotNil:
		[Transcript cr; show: class name,'>>', selector, '(', name,' is shadowed)']