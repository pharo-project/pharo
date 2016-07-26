accessing
title

	^ 'Message #', targetMethodName, ' not preceded by ',
		(condition beforeDescriptionString truncateWithElipsisTo: 30)