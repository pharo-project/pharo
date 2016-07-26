compatibility-tools
openLabel: aLabel
	| page |

	page := GTPlayPage new
		title: aLabel;
		yourself.
	^ self openOn: page