copying
copy: sourceRectangle from: sourceForm to: destPt rule: rule
	^ self copy: (destPt extent: sourceRectangle extent)
		from: sourceRectangle topLeft in: sourceForm rule: rule