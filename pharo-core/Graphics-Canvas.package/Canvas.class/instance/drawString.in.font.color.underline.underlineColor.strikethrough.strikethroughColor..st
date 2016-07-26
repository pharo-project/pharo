drawing-text
drawString: s in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc
	^self drawString: s from: 1 to: s size in: boundsRect font: fontOrNil color: c underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc