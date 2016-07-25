I read and write BMP files.

Example to save and load a screenshot of the world in a .bmp file:

BMPReadWriter 
   putForm: (Form fromDisplay: (0@0 corner: 400@400))
   onFileNamed: '/tmp/screenshot.bmp'.

(ImageMorph withForm: (BMPReadWriter formFromFileNamed: '/tmp/screenshot.bmp')) openInWindow.