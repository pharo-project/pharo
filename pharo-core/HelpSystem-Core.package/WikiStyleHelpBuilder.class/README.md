I'm an HelpSystem builder which create an HelpSystem book from a wiki-like formatted string. 

! Usage

- Create a class-side method on the class you want to put documentation.
- Add the pragma <wikiStyleHelp:> with the title of the book as parameter
- The method should answer the documentation as a String (using Pier syntax. See http://www.piercms.com/doc/syntax ). 

! Example

MyClass class>>aMethod
        <wikiStyleHelp: #'The name of the book I want to write'>
        ^ '
 ! Section 1

 Some text for this section

 !! Subsection 1.1
 !!! Subsection 1.1.1
 !! Subsection 1.2
 ! Section 2'