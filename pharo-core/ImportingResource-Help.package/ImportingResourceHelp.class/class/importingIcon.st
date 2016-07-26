documentation
importingIcon
	^ HelpTopic 
		title: 'Importing Icon'
		contents: 
'The easiest way to create icons from a bunch of .png pictures is to use IconFactory. 

1 - You can load it from the Catalog Browser, available from the WorldMenu/Tools.

2 - Once loaded, you need to create an empty class, which will contains your icons. Create a subclass of Object. For the purpose of the example, we will call it MatrixCubeIcons 

3 - You need to prepare the icon class to handle icons. For example, to provide caches. Simply evaluate:
	IconFactory setup: MatrixCubeIcons
	
4 - The last step is to import the .png files in the icon class:
	IconFactory createIconsFromDirectory: ''/Users/alexandrebergel/Desktop/Cube'' inClass: MatrixCubeIcons

After this last step, the class MatrixCubeIcons will contains the icons found in the directory /Users/alexandrebergel/Desktop/Cube
The icon class should be packaged within your application. 

Note that icons classes are GT-compatible. Simply inspect the expression: MatrixCubeIcons current

 '