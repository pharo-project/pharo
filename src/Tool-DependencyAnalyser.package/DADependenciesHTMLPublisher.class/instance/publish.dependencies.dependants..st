publishing
publish: aPackageName dependencies: dependencies dependants: dependants
	| dependantLinks |
	dependantLinks := dependants sorted collect: [ :name | '<a href="#', name , '">' , name , '</a>' ].
	
	stream << 
'						<tr>
							<td id="' << aPackageName << '">' << (self htmlForPackage: aPackageName) << '</td>
							<td>' << (self sizeBadgeFor: dependencies) << Character space 
							<< (self htmlForDependencies: dependencies sorted of: aPackageName) << Character space
							<< (self htmlForIgnoredDependenciesOf: aPackageName) << '</td>
							<td>' << (self sizeBadgeFor: dependants) << Character space << (Character space join: dependantLinks) << '</td>
						</tr>'; lf.