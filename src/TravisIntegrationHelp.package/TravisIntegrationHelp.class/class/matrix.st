pages
matrix
	^ HelpTopic 
		title: 'Matrix Configuration'
		contents: 
		(self heading: 'Matrix Configuration'),	
		'It may happen that you need different configuration. For example, having a development configuration and a stable one. The development configuration usually consider the latest version of your packages, while the stable uses particular versions. 
		
You would need to create a new ', (self bold: '.smalltalk.ston'), ' file. To follow with our example, we have introduced this file in Step 04. We will now define a new file, called ', (self bold: '.smalltalk_stable.ston'), ' with the following content:
',
(self bold: '
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #configuration : ''Roassal2'',
      #repository : ''http://smalltalkhub.com/mc/ObjectProfile/Roassal2/main/'',
      #load : [ ''default'' ],
      #platforms : [ #pharo ],
      #version: #stable
    }
  ],
  
  #testing : {
  	#coverage : {
  	 #categories : [ ''Roassal2-AttachPoint-Tests'', ''Roassal2-Builder-AnimatedScatterPlot'', ''Roassal2-Builder-ApplicationMatrix'', ''Roassal2-Builder-CircularMap'', ''Roassal2-Builder-Common'', ''Roassal2-Builder-Composer'', ''Roassal2-Builder-DSM'', ''Roassal2-Builder-Explora'', ''Roassal2-Builder-Grapher-Decorators-Tests'', ''Roassal2-Builder-Grapher-Scaling-Tests'', ''Roassal2-Builder-Grapher-Tests'', ''Roassal2-Builder-Kiviat'', ''Roassal2-Builder-Legend'', ''Roassal2-Builder-Map-Tests'', ''Roassal2-Builder-Mondrian'', ''Roassal2-Builder-MondrianOLD'', ''Roassal2-Builder-NameCloud'', ''Roassal2-Builder-PieChart'', ''Roassal2-Builder-ScrollBar'', ''Roassal2-Builder-Simon'', ''Roassal2-Builder-Spectrograph'', ''Roassal2-Builder-StackBarPlot'', ''Roassal2-Builder-TextBuilder'', ''Roassal2-Builder-Timeline-Tests'', ''Roassal2-Builder-TreeMap'', ''Roassal2-Builder-UI-Slider'', ''Roassal2-Builder-UML'',  ''Roassal2-ColorNormalizer-Tests'', ''Roassal2-Core-Tests'', ''Roassal2-Exporter-SVG'', ''Roassal2-Interactions'', ''Roassal2-Exporter-PNG'', ''Roassal2-Exporter-HTML5'', ''Roassal2-Interactions-Resize-Tests'', ''Roassal2-Layouts-Tests'', 
''Roassal2-LineDecorations'', ''Roassal2-MorphSpecific'', ''Roassal2-OpenStreetMap'', ''Roassal2-Plugins'', ''Roassal2-Sparkline'', ''Roassal2-Table'', ''Roassal2-Tests'', ''Roassal2-Utility'', ''Trachel-MorphSpecific'', ''Trachel-Tests'', ''Trachel-Viva'', ''Roassal2GT-Glamour'', ''Roassal2GT-Glamour-Actions'', ''Roassal2GT-Glamour-Context'', ''Roassal2GT-Glamour-Roassal-Interaction'', ''Roassal2GT-Tests-Examples''
 ]
   	},
    
    #exclude : {
        #classes : [ #RTCacheTest , #Roassal2VWTest ]
    }
   }
}
'),
'

The principal difference between this file and ', (self bold: '.smalltalk.ston'), ' is the ', (self bold: ' #version: #stable'),
'

We are now ready to tell to Travis that we want to use this new file in some particular conditions, defined with a matrix. 
The ', (self bold: '.travis.yml'), ' should be:
',
(self bold: 
'language: smalltalk

os:
  - osx
  - linux

smalltalk:
  - Pharo-6.0
  - Pharo-5.0

matrix:
    include: 
        - smalltalk: Pharo-6.0
          smalltalk_config: .smalltalk_stable.ston
          os: osx
        - smalltalk: Pharo-5.0
          smalltalk_config: .smalltalk_stable.ston
          os: osx
        - smalltalk: Pharo-6.0
          smalltalk_config: .smalltalk_stable.ston
          os: linux
        - smalltalk: Pharo-5.0
          smalltalk_config: .smalltalk_stable.ston
          os: linux

'),
'

With this new content, Travis will run different configurations.'