private
completedWidth

	| range fraction progress |
	range := end - start.
	progress := value - start.
	fraction := progress / range.
	^ (fraction * self totalBarWidth) truncated.