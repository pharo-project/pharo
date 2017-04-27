pages
step04
	^ HelpTopic 
		title: 'Step 04 - Define .smalltalk.ston'
		contents: 'In your Git repository you will need to add two files, .smalltalk.ston and .travis.yml (yes, with a period at the begining).
		
#configuration is the name of your project. Travis will look for ConfigurationOfRoassal2 in that case
#repository where the configuration is kept
#load is the Metacello Configuration group, usually defined in the baseline
#version 
#version corresponds to the name of a baseline 
#categories contains a list of class categories (also called tags) that contains unit tests.

-=-=-=-=-=
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #configuration : ''Roassal2'',
      #repository : ''http://smalltalkhub.com/mc/ObjectProfile/Roassal2/main/'',
      #load : [ ''default'' ],
      #platforms : [ #pharo ],
      #version: #development
    }
  ],

  #testing : {
   #categories : [ ''Roassal2-AttachPoint-Tests'', ''Roassal2-Builder-AnimatedScatterPlot'', ''Roassal2-Builder-ApplicationMatrix'', ''Roassal2-Builder-CircularMap'', ''Roassal2-Builder-Common'', ''Roassal2-Builder-Composer'', ''Roassal2-Builder-DSM'', ''Roassal2-Builder-Explora'', ''Roassal2-Builder-Grapher-Decorators-Tests'', ''Roassal2-Builder-Grapher-Scaling-Tests'', ''Roassal2-Builder-Grapher-Tests'', ''Roassal2-Builder-Kiviat'', ''Roassal2-Builder-Legend'', ''Roassal2-Builder-Map-Tests'', ''Roassal2-Builder-Mondrian'', ''Roassal2-Builder-MondrianOLD'', ''Roassal2-Builder-NameCloud'', ''Roassal2-Builder-PieChart'', ''Roassal2-Builder-ScrollBar'', ''Roassal2-Builder-Simon'', ''Roassal2-Builder-Spectrograph'', ''Roassal2-Builder-StackBarPlot'', ''Roassal2-Builder-TextBuilder'', ''Roassal2-Builder-Timeline-Tests'', ''Roassal2-Builder-TreeMap'', ''Roassal2-Builder-UI-Slider'', ''Roassal2-Builder-UML'',  ''Roassal2-ColorNormalizer-Tests'', ''Roassal2-Core-Tests'', ''Roassal2-Exporter-SVG'', ''Roassal2-Interactions'', ''Roassal2-Exporter-PNG'', ''Roassal2-Exporter-HTML5'', ''Roassal2-Interactions-Resize-Tests'', ''Roassal2-Layouts-Tests'', 
''Roassal2-LineDecorations'', ''Roassal2-MorphSpecific'', ''Roassal2-OpenStreetMap'', ''Roassal2-Plugins'', ''Roassal2-Sparkline'', ''Roassal2-Table'', ''Roassal2-Tests'', ''Roassal2-Utility'', ''Trachel-MorphSpecific'', ''Trachel-Tests'', ''Trachel-Viva'', ''Roassal2GT-Glamour'', ''Roassal2GT-Glamour-Actions'', ''Roassal2GT-Glamour-Context'', ''Roassal2GT-Glamour-Roassal-Interaction'', ''Roassal2GT-Tests-Examples''
 ]
   }
}
-=-=-=-=-=
'