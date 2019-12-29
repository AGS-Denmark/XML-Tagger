# XML tagger 
XML tagger is at program that interts XML tags into a F# file. 
it will not owerwhrite any current XML tags and responds to the folowing:
- 2x new line and a "let"
- "type"
- "member"
- "//XML"

it will register indput types and return type and insert this into the file. 
it will create a new file.

HOW TO RUN:
dependencies
- mono 
- F#

fype folowing commands in terminal:

- fsharpc -a XML-Tagger.fs
- fsharpc -r XML-Tagger.dll XMLrunner.fsx
- mono XMLrunner.exe

the programe will guide you from here..
