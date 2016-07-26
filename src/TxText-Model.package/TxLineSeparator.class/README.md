I represent a line separator
(NOT a new line in text).

My size is always 1. And my textual representation is always Character cr.

I have only two valid span positions:
 0 - denotes the last valid position of the previous line (line end)
 1 - denotes the first valid position of the next line (line start).

My attributes define the default attributes of the new line, so even if the next span
is a line separator (meaning that the next line is empty), I can provide information about visual properties such as line height etc.
