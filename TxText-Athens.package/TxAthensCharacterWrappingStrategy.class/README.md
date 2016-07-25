This layout strategy wraps text, so it never overflows the view horizontally.
If the line is too long to be shown fully as a single text line in view,
it is automatically wrapped to the next line.

Therefore, the layout is calculated based on the view's extent.
The 'rightMargin' can be used to wrap the text before it gets wider than view width, e.g:

maxLineWidth := view width - rightMargin.

By default, right margin is Zero, meaning the max line width is same as view width.

Note, if you use this strategy, the view must never use horizontal scrolling,
and its offset x must be always Zero (since it makes little sense to have otherwise with such a strategy).