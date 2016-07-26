accessing
title

	^ 'Message #', targetMethodName, ' not followed by ',
		(condition afterDescriptionString truncateWithElipsisTo: 30)