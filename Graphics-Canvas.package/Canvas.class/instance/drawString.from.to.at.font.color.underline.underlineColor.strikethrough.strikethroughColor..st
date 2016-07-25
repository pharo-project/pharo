drawing-text
drawString: s from: firstIndex to: lastIndex at: pt font: font color: aColor underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc
	self drawString: s from: firstIndex to: lastIndex in: (pt extent: 10000@10000) font: font color: aColor underline: underline underlineColor: uc strikethrough: strikethrough strikethroughColor: sc