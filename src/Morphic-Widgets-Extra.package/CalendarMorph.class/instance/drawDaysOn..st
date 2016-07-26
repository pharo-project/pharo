drawing
drawDaysOn: aCanvas

	days do: [:each |
		each
			drawOn: aCanvas
			offset: self bounds topLeft]