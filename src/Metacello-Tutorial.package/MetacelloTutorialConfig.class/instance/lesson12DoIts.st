lessons
lesson12DoIts
"  [ see methods: #version12: #preloadForCore #postloadForCore:package: ]
Occassionally, you find that you need to perform an expression either before a package is loaded, or
after a package is loaded. To do that in Metacello, you can define a preLoadDoIt selector and a 
postLoadDoIt selector:

	(MetacelloTutorialConfig project version: '1.2') spec.

If you open a Transcript and execute the following expression, you will see that the pre load and
post load methods were executed:

	(MetacelloTutorialConfig project version: '1.2') load.

The pre/post load methods may take 0, 1 or 2 args. The loader is the first optional argument and the 
loaded packageSpec is the second optional argument.
"