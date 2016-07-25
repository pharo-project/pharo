Read an animated GIF file.

Example: open all images of an animated GIF file in a Morphic window

gifPath :=  '/path/to/my_animated.gif'.
forms := (AnimatedGIFReadWriter formsFromFileNamed: gifPath) forms.
content := UITheme builder newRow: (forms collect: [:aForm| UITheme builder newImage: aForm]).
content openInWindowLabeled: 'Content of ', gifPath.