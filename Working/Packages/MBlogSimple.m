(* ::Package:: *)

BeginPackage["MBlogSimple`"]

MBlogSimple::usage =
        "\:5c06\:7b14\:8bb0\:672c\:751f\:6210Github\:535a\:5ba2\:9875\:9762\:53ca\:4e0a\:4f20GitHub\:7b49";


(* ::Section::Closed:: *)
(*Usage*)


nb2Index::usage="\:751f\:6210\:4e3b\:9875\:7684\:7f51\:9875";
nb2Post::usage="\:751f\:6210\:65e5\:5fd7\:7684\:7f51\:9875";


buildSite::usage="\:628a\:6240\:6709\:7684nb\:6587\:4ef6\:91cd\:65b0\:751f\:6210\:4e00\:6b21HTML\:ff0c\:76f8\:5f53\:4e8e\:91cd\:65b0\:5efa\:7ad9\:ff0c\:8f83\:6162\:ff0c\:5f85\:6539\:8fdb\:ff0c\:9002\:5408\:8001\:65e5\:5fd7\:7684\:8fc1\:79fb";
buildSiteSimple::usage="\:53ea\:5bf9\:6ca1\:6709\:751f\:6210HTML\:7684nb\:6587\:4ef6\:91cd\:65b0\:751f\:6210\:4e00\:6b21HTML";
cleanSite::usage="\:628a\:6240\:6709\:7684nb\:6587\:4ef6\:6ca1\:6709\:5bf9\:5e94\:7684\:7f51\:9875\:6709\:76f8\:5173\:5185\:5bb9\:5220\:9664";
refreshSite::usage="\:6bd4\:5982\:628a\:6240\:6709\:7684\:65e5\:5fd7\:7684\:4fa7\:8fb9\:680f\:66f4\:65b0";


nbBlogDelete::usage="\:628a\:76f8\:5173\:65e5\:5fd7\:5220\:9664";


Begin["`Private`"];


(* ::Section::Closed:: *)
(*\:7f51\:9875\:8f93\:51fa\:51fd\:6570*)


(* ::Subsection:: *)
(*\:4e3b\:9875Index*)


(* ::Text:: *)
(*nb2Index*)


(* ::Subsection:: *)
(*\:9875\:9762Page*)


(* ::Text:: *)
(*nb2Tags*)


(* ::Text:: *)
(*...*)


(* ::Subsection:: *)
(*\:65e5\:5fd7Post*)


(* ::Text:: *)
(*nb2Post*)


(* ::Section::Closed:: *)
(*\:5355\:5143\:8f6c\:5316\:51fd\:6570*)


inputBox2String=First[FrontEndExecute[FrontEnd`ExportPacket[Cell[BoxData[#],"Input"],"InputText"]]]&;


(* ::Section:: *)
(*Common*)


(* ::Text:: *)
(*AL\:8868\:793aAbsoluteLocal\:8def\:5f84\:ff0c\:5373\:672c\:5730\:7684\:7edd\:5bf9\:8def\:5f84*)


pathFormatUnix:=StringReplace[#1,"\\"->"/"]&;


fileNameJoinUnix:=FileNameJoin[#1,OperatingSystem->"Unix"]&;


take[list_,n_]:=If[Length[list]>10,Take[list,10],list];


htmlStringTrim[x_]:=Block[{count},(count=StringCount[x,"style"];StringReplace[x,p:(Repeated[Shortest["style=\""~~___],{count}])~~"\"":>StringJoin[Riffle[StringSplit[p,{"\" style=\""}],"; ",{2,-1,2}]]<>"\""])]
(*\:6b64\:51fd\:6570\:4fee\:6b63\:4e00\:4e2aLinux\:7248\:768410.1\:4ee5\:4e0b\:7248\:672c\:4e2dExport as HTMLFragment\:7684\:4e00\:4e2abug, 10.1\:7248\:53ef\:4e0d\:7528*)


exportXML:=Export[#1,#2,"XML","AttributeQuoting"->"\"",CharacterEncoding->"UTF8"]&;


deleteDirectoryForce:=If[DirectoryQ@#,DeleteDirectory[#,DeleteContents->True]]&;
reCreateDirectoryForce:=If[DirectoryQ@#,DeleteDirectory[#,DeleteContents->True];CreateDirectory@#,CreateDirectory@#]&;
copyDirectoryForce:=If[DirectoryQ@#2,
DeleteDirectory[#2,DeleteContents->True];CopyFile[#1,#2],CopyFile[#1,#2]]&;


(* ::Subsection:: *)
(*Path*)


(* ::Text:: *)
(*\:5b9e\:9645\:4e0a\:56e0\:4e3a\:4e0d\:4e00\:5b9a\:6709GitHub\:76ee\:5f55\:ff0c\:6bd4\:5982\:6211\:5728Ubuntu\:91cc\:6d4b\:8bd5\:65f6\:5c31\:6ca1\:6709\:5efa\:7acbGitHub\:6587\:4ef6\:5939\:ff0c\:800c\:662f\:53ea\:5efa\:7acb\:4e86MBlogSimple*)


(*dirBlog[AbsoluteLocal]=pathFormatUnix@fileNameJoinUnix@{$UserDocumentsDirectory,"GitHub","MBlogSimple"};*)


(*dirBlog[Relative]="MBlogSimple";*)


(* ::Text:: *)
(*\:6b64\:76ee\:5f55\:4e3a\:624b\:52a8\:4fee\:6539\:ff0c\:6216Install\:65f6\:624b\:52a8\:8f93\:5165\:53d8\:91cf\:540e\:81ea\:52a8\:4fee\:6539\:3002*)


dirBlogHome[AL]=pathFormatUnix@"C:\\Users\\HyperGroups\\Documents\\GitHub\\MBlogSimple";


dirBlogPush[AL]=dirBlogHome[AL];


dirBlogSite[AL]=fileNameJoinUnix[{dirBlogHome[AL],"Site"}];


dirBlogWorking[AL]=fileNameJoinUnix[{dirBlogHome[AL],"Working","Working"}];


(*dirBlogPush[AL]=pathFormatUnix@fileNameJoinUnix@{$UserDocumentsDirectory,"GitHub","MBlogSimple"};
(*Push\:7684\:76ee\:5f55\:53ef\:4ee5\:8bbe\:7f6e\:6210\:4e0d\:540c\:7684\:ff0c\:56e0\:6b64\:6709\:8fd9\:91cc\:7684\:547d\:540d\:ff0c\:56e0\:4e3a\:6b64\:5904\:81ea\:52a8\:7248\:662f\:8bbe\:7f6e\:6210\:76f8\:540c\:7684\:76ee\:5f55\:ff0c\:56e0\:6b64Push\:6807\:8bb0\:7684\:6ca1\:6709\:663e\:793a\:51fa\:5dee\:5f02\:3002*)*)


dirLayout[AL]=fileNameJoinUnix[{dirBlogHome[AL],"Layout"}];


dirNotebook[AL]=fileNameJoinUnix[{dirBlogHome[AL],"Notebooks"}];


fileRecentPost[AL]=fileNameJoinUnix[{dirLayout[AL],"final","recentPostNew.xml"}];


(* ::Section:: *)
(*\:529f\:80fd\:51fd\:6570*)


(* ::Subsection:: *)
(*RecentPost\:66f4\:65b0*)


recentPostUpdate[]:=Module[{n},

files[Notebook]=pathFormatUnix@FileNames["*.nb",dirNotebook[AL],\[Infinity]];
filesSorted[Notebook]=(SortBy[files[Notebook],Date/.FileInformation[#]&]//Reverse);

recentPostGen[n_]:=Take[filesSorted[Notebook],n];
recentPostBaseNameGen[n_]:=FileBaseName/@recentPostGen[n];
recentPostHyperlinkGen[n_]:=Take[recentPostList[Hyperlink],n];

filesSortedRelative[Notebook]=pathFormatUnix@FileNameDrop[#,Length[FileNameSplit@dirNotebook[AL]]]&/@filesSorted[Notebook];
(*recentPostBaseNameList[RightBar]=recentPostBaseNameGen[10];
recentPostBaseNameList[Index]=recentPostBaseNameGen[25];*)
var[postCount]=files[Notebook]//Length;

filesBaseNameSorted[Notebook]=recentPostBaseNameGen[{1,-1}];
filesSite[AL]=StringReplace[#,{"/Notebooks/"->"/Site/",FileBaseName[#]<>".nb"->fileNameJoinUnix[{FileBaseName[#],FileBaseName[#]<>".html"}]}]&/@filesSorted[Notebook];
recentPostList[Hyperlink]=MapThread[Hyperlink,{filesBaseNameSorted[Notebook],filesSite[AL]}];


recentPostOutput[RightBar]=(If[(n=Length[recentPostList[Hyperlink]])>10,n=10];

Column[MapIndexed[Item[Style[take[recentPostList[Hyperlink],n][[#2//Last]],FontColor->ColorNegate@RGBColor[1-#],FontSize->20,Bold],Background->RGBColor[.2#],ItemSize->{30,5}]&,RandomReal[1,{n,3}]]]);

recentPostOutputString[RightBar]=htmlStringTrim@FromCharacterCode[ToCharacterCode[ExportString[recentPostOutput[RightBar],"HTMLFragment"]],"UTF8"];

Export[pathFormatUnix[dirLayout[AL]<>"\\recentPosts.xml"],recentPostOutputString[RightBar],"Text"];



]


(* ::Subsection:: *)
(*Category\:66f4\:65b0*)


categoryUpdate[]:=Module[{n},

categoryList=(FileNameDrop/@filesSortedRelative[Notebook])//Union;

categoryOutput[RightBar]=(If[(n=Length[categoryList])>20,n=20];Column[MapIndexed[Item[Style[take[categoryList,n][[#2//Last]],FontColor->ColorNegate@RGBColor[1-#],FontSize->20,Bold],Background->RGBColor[.2#],ItemSize->{30,5}]&,RandomReal[1,{n,3}]]]);

categoryOutputString[RightBar]=htmlStringTrim@FromCharacterCode[ToCharacterCode[ExportString[categoryOutput[RightBar],"HTMLFragment"]],"UTF8"];

Export[pathFormatUnix[dirLayout[AL]<>"\\category.xml"],categoryOutputString[RightBar],"Text"];

]


(* ::Subsection:: *)
(*\:4e3b\:9875\:66f4\:65b0*)


indexUpdate[]:=Module[{rules},
recentPostUpdate[];
recentPostOutput[Index]=(If[(n=Length[recentPostList[Hyperlink]])>10,n=10];

Column[MapIndexed[Item[Style[take[recentPostList[Hyperlink],n][[#2//Last]],FontColor->ColorNegate@RGBColor[1-#],FontSize->20,Bold],Background->RGBColor[.2#],ItemSize->{30,5}]&,RandomReal[1,{n,3}]]]);

recentPostOutputString[Index]=htmlStringTrim@FromCharacterCode[ToCharacterCode[ExportString[recentPostOutput[Index],"HTMLFragment"]],"UTF8"];
recentPostXML[Index]=ImportString[recentPostOutputString[Index],"XML"][[2]];

indexFile=pathFormatUnix@FileNameJoin[{dirBlogHome[AL],"index.old.html"}];
indexImported=Import[indexFile,"XMLObject"];
(*---\:4e5f\:53ef\:4ee5\:8f93\:51fa\:5230Site\:76ee\:5f55\:ff0c\:53ea\:662f\:6253\:5f00\:4e00\:4e2a\:6587\:4ef6\:5939Site\:518d\:70b9\:5f00\:4e3b\:9875\:7565\:4e0d\:65b9\:4fbf\:ff0c\:56e0\:6b64\:8fd9\:91cc\:5148\:4f7f\:7528.Local\:6807\:8bc6*)
indexExport=indexImported/.XMLElement["section",{"class"->"main-content"},{}]->Sequence@@{XMLElement["section",{"class"->"main-content"},{}],XMLElement["div",{},{recentPostXML[Index]}]};
Export[StringReplace[indexFile,".old"->".Local"],indexExport,"XML"];


rules=XMLElement["a",{"href"->x__},y___]:>XMLElement["a",{"href"->StringJoin["/MBlogSimple/",FileBaseName@x]},y];
indexExport=indexImported/.XMLElement["section",{"class"->"main-content"},{}]->Sequence@@{XMLElement["section",{"class"->"main-content"},{}],XMLElement["div",{},{recentPostXML[Index]/.rules}]};
Export[StringReplace[indexFile,".old"->""],indexExport,"XML"];
]


(* ::Subsection:: *)
(*BuildSite*)


(* ::Text:: *)
(*\:76ee\:524d\:7684\:8bbe\:8ba1\:4e2d\:ff0c\:65b0\:5efa\:7acb\:4e00\:7bc7\:65e5\:5fd7\:ff0c\:7136\:540e\:53d1\:5e03\:540e\:4f1a\:751f\:6210\:4e00\:4e2a\:7f51\:9875\:ff0c\:4f46\:662f\:5982\:679c\:628a\:4e00\:4e9b\:8001\:7684nb\:6587\:4ef6\:590d\:5236\:5230Notebooks\:6587\:4ef6\:5939\:ff0c\:5219\:53ef\:4ee5\:8c03\:7528\:6b64\:51fd\:6570\:91cd\:65b0\:5efa\:7ad9\:ff0c\:76f8\:5f53\:4e8e\:628a\:6240\:6709nb\:6587\:4ef6\:5747\:91cd\:65b0\:751f\:6210\:7f51\:9875\:ff0c\:6682\:65f6\:53ef\:80fd\:901f\:5ea6\:8f83\:6162\:3002\:6709\:7684\:5730\:65b9\:ff0c\:6bd4\:5982\:5149\:6805\:5316\:5355\:5143\:4e3a\:56fe\:7247\:7684\:65f6\:95f4\:4e0d\:5bb9\:6613\:6539\:8fdb\:ff0c\:5176\:4ed6\:6709\:4e00\:4e9b\:662f\:53ef\:4ee5\:540e\:671f\:4f18\:5316\:7684\:3002\:6bd4\:5982\:4e00\:7bc7\:65e5\:5fd710\:79d2\:949f\:ff0c\:90a3100\:7bc7\:53ef\:80fd\:5c31\:898116\:5206\:949f\:4e86\:ff0c\:5c31\:76f8\:5bf9\:8f83\:6162\:4e86\:3002*)


(* ::Text:: *)
(*\:53ef\:4ee5\:6539\:8fdb\:7684\:662f\:5f15\:5165\:91cd\:540d\:6587\:4ef6\:7684\:81ea\:52a8\:91cd\:547d\:540d\:7b49\:3002force->True\:9009\:9879\:5c31\:4e0d\:4f1a\:5f39\:51fa\:76f8\:540c\:540d\:79f0\:7684nb\:6587\:4ef6\:5df2\:5b58\:5728\:7684\:95ee\:9898\:ff0c\:4f1a\:76f4\:63a5\:8986\:76d6\:539fnb\:6587\:4ef6\:3002*)


Options[buildSite]={"force"->False};


buildSite[x___,OptionsPattern[]]:=Module[{nb,notebooksImported},

notebooksImported=Import[#,"Notebook"]&/@
filesSorted[Notebook];
var[Force]=OptionValue["force"];
Do[nb=CreateDocument[notebooksImported[[i]]];If[var[Force]==True,nb2Post[nb,"class"->FileNameDrop[filesSortedRelative[Notebook][[i]]],"force"->True],nb2Post[nb,"class"->FileNameDrop[filesSortedRelative[Notebook][[i]]]]];NotebookClose[nb];,{i,Length@notebooksImported}];

]


(* ::Subsection:: *)
(*cleanSite*)


(* ::Text:: *)
(*\:5f53\:5220\:9664\:67d0\:4e9bnb\:6587\:4ef6\:65f6\:ff0c\:8fd0\:884c\:6b64\:51fd\:6570\:ff0c\:4f1a\:76f8\:5e94\:5220\:9664\:76f8\:5173\:7684HTML\:6587\:4ef6\:3002*)
(*\:6bd4\:5982\:ff0c\:624b\:52a8\:8fdb\:5165Notebook\:76f8\:5173\:76ee\:5f55\:5220\:9664\:4e00\:4e9b\:65e7\:7684\:65e5\:5fd7\:6587\:6863\:8fd9\:79cd\:64cd\:4f5c\:65f6\:3002*)


cleanSite[]:=Module[{filesSorted,filesKeep,filesToDelete,dirsKeep,dirsToDelete,dirsExclude},
(*\:5220\:9664Working\:76ee\:5f55\:4e0b\:7684\:65e0\:6548\:6587\:4ef6*)
filesSorted[Working]=fileNameJoinUnix[{dirBlogWorking[AL],StringTrim[#,".nb"],FileBaseName[#]<>".html"}]&/@(filesSortedRelative[Notebook]//Flatten)//Union;
filesKeep[Working]=Join[pathFormatUnix@FileNames["*.html",dirBlogWorking[AL]],filesSorted[Working]];
filesToDelete[Working]=Complement[pathFormatUnix@FileNames["*.html",dirBlogWorking[AL],\[Infinity]],filesKeep[Working]];
dirsToDelete[Working]=FileNameDrop/@filesToDelete[Working];
(*\:5220\:9664Site\:76ee\:5f55\:4e0b\:7684\:65e0\:6548\:6587\:4ef6*)
filesSorted[Site]=fileNameJoinUnix[{dirBlogSite[AL],StringTrim[#,".nb"],FileBaseName[#]<>".html"}]&/@(filesSortedRelative[Notebook]//Flatten)//Union;
filesKeep[Site]=Join[pathFormatUnix@FileNames["*.html",dirBlogSite[AL]],filesSorted[Site]];
filesToDelete[Site]=Complement[pathFormatUnix@FileNames["*.html",dirBlogSite[AL],\[Infinity]],filesKeep[Site]];

(*\:5220\:9664Push\:76ee\:5f55\:4e0b\:7684\:65e0\:6548\:6587\:4ef6,\:4ee5\:6587\:4ef6\:5939\:4e3a\:5355\:4f4d\:5220\:9664*)dirsExclude=Select[Import[fileNameJoinUnix[{dirBlogHome[AL],"exclude.txt"}],"List",CharacterEncoding->"CP936"],StringFreeQ[#,StartOfString~~"--"]&];
filesSorted[Push]=fileNameJoinUnix[{dirBlogHome[AL],FileBaseName[#]<>"/index.html"}]&/@(filesSortedRelative[Notebook]//Flatten)//Union;
dirsKeep0[Push]=pathFormatUnix[FileNameDrop[#]]&/@filesSorted[Push];
dirsKeep[Push]=(fileNameJoinUnix[{dirBlogHome[AL],#}]&/@(dirsExclude~Join~{"Working","assets","HTMLFiles","Includes","Layout","Notebooks","Pages","Plugins","Site","stylesheets",".git"}))~Join~dirsKeep0[Push];
dirsToDelete[Push]=Complement[Select[pathFormatUnix@FileNames["*",dirBlogHome[AL]],DirectoryQ],dirsKeep[Push]];

DeleteFile[filesToDelete[Site]];
DeleteDirectory[dirsToDelete[Working],DeleteContents->True];
DeleteDirectory[dirsToDelete[Push],DeleteContents->True];
]


(* ::Text:: *)
(*\:6392\:9664\:4e00\:4e9b\:76ee\:5f55\:ff0c\:5220\:9664\:6ca1\:6709\:76f8\:5e94nb\:6587\:4ef6\:7684\:76ee\:5f55\:ff0c\:5220\:9664\:7a7a\:76ee\:5f55\:3002*)


(* ::Text:: *)
(*\:76ee\:524d\:7684\:8bbe\:8ba1\:4e2d\:ff0cSite\:76ee\:5f55\:53ea\:6709\:4e00\:4e2a\:7f51\:9875\:6587\:4ef6\:ff0c\:56e0\:6b64\:53ea\:8981\:628a\:7f51\:9875\:5220\:9664\:5373\:53ef\:3002*)


(* ::Text:: *)
(*\:6ce8\:610f\:6709\:4e2aBuildSite\:7684bug\:ff0c\:628a\:6240\:6709\:6587\:4ef6\:90fd\:91cd\:65b0\:653e\:5728Mathematica\:76ee\:5f55\:4e86\:3002*)


(* ::Input:: *)
(*cleanSiteTest[]:=Module[{},*)
(*(*\:5220\:9664Working\:76ee\:5f55\:4e0b\:7684\:65e0\:6548\:6587\:4ef6*)*)
(*filesSorted[Working]=fileNameJoinUnix[{dirBlogWorking[AL],StringTrim[#,".nb"],FileBaseName[#]<>".html"}]&/@(filesSortedRelative[Notebook]//Flatten)//Union;*)
(*filesKeep[Working]=Join[pathFormatUnix@FileNames["*.html",dirBlogWorking[AL]],filesSorted[Working]];*)
(*filesToDelete[Working]=Complement[pathFormatUnix@FileNames["*.html",dirBlogWorking[AL],\[Infinity]],filesKeep[Working]];*)
(**)
(*(*\:5220\:9664Site\:76ee\:5f55\:4e0b\:7684\:65e0\:6548\:6587\:4ef6*)*)
(*filesSorted[Site]=fileNameJoinUnix[{dirBlogSite[AL],StringTrim[#,".nb"],FileBaseName[#]<>".html"}]&/@(filesSortedRelative[Notebook]//Flatten)//Union;*)
(*filesKeep[Site]=Join[pathFormatUnix@FileNames["*.html",dirBlogSite[AL]],filesSorted[Site]];*)
(*filesToDelete[Site]=Complement[pathFormatUnix@FileNames["*.html",dirBlogSite[AL],\[Infinity]],filesKeep[Site]];*)
(**)
(*(*\:5220\:9664Push\:76ee\:5f55\:4e0b\:7684\:65e0\:6548\:6587\:4ef6,\:4ee5\:6587\:4ef6\:5939\:4e3a\:5355\:4f4d\:5220\:9664*)dirsExclude=Select[Import[fileNameJoinUnix[{dirBlogHome[AL],"exclude.txt"}],"List",CharacterEncoding->"CP936"],StringFreeQ[#,StartOfString~~"--"]&];*)
(*filesSorted[Push]=fileNameJoinUnix[{dirBlogHome[AL],FileBaseName[#]<>"/index.html"}]&/@(filesSortedRelative[Notebook]//Flatten)//Union;dirsKeep0[Push]=pathFormatUnix[FileNameDrop[#]]&/@filesSorted[Push];*)
(*dirsKeep[Push]=(fileNameJoinUnix[{dirBlogHome[AL],#}]&/@(dirsExclude~Join~{"Working","assets","HTMLFiles","Includes","Layout","Notebooks","Pages","Plugins","Site","stylesheets",".git"}))~Join~dirsKeep0[Push];*)
(*dirsToDelete[Push]=Complement[Select[pathFormatUnix@FileNames["*",dirBlogHome[AL]],DirectoryQ],dirsKeep[Push]];*)
(**)
(*]*)


(* ::Subsection:: *)
(*refreshSite*)


(* ::Text:: *)
(*\:5220\:9664\:6bd4\:5982\:751f\:6210\:7684\:7f51\:9875\:4e2d\:7684\:4fa7\:8fb9\:680f\:4e2d\:7684\:65e0\:6548\:65e5\:5fd7\:5217\:8868\:ff0c\:5982\:66f4\:65b0\:6240\:6709\:65e5\:5fd7\:7684\:4fa7\:8fb9\:680f\:6a21\:5757\:3002*)


refreshSite[]:=Module[{rightBarNew,htmlFiles,htmlFilesImported,htmlFilesToExport},
cleanSite[];
(*\:66f4\:65b0Site\:76ee\:5f55\:7684\:7f51\:9875*)
htmlFiles=FileNames["*.html",dirBlogSite[AL],\[Infinity]];
htmlFilesImported=Import[#,"XMLObject"]&/@htmlFiles;
rightBarNew=Import[FileNameJoin[{dirLayout[AL],"final","recentPostNew.xml"}],"XMLObject"][[2]];
Do[htmlFilesToExport=htmlFilesImported[[i]]/.XMLElement["div",{"class"->"span4"},x___]->rightBarNew;
Export[htmlFiles[[i]],htmlFilesToExport[[2]],"XML"],{i,Length@htmlFilesImported}];

(*\:66f4\:65b0Push\:76ee\:5f55\:7684\:7f51\:9875\:ff0c\:5373MBlogSimple\:76ee\:5f55*)
htmlFiles=Select[fileNameJoinUnix[{#,"index.html"}]&/@dirsKeep0[Push],FileExistsQ];
htmlFilesImported=Import[#,"XMLObject"]&/@htmlFiles;
(*\:4fa7\:8fb9\:680f\:7684\:8def\:5f84\:66f4\:65b0*)rules=XMLElement["a",{"href"->x__},y___]:>XMLElement["a",{"href"->StringJoin["/MBlogSimple/",FileBaseName@x]},y];
rightBarNew=Import[fileRecentPost[AL],"XMLObject"][[2]]/.rules;
Do[htmlFilesToExport=htmlFilesImported[[i]]/.XMLElement["div",{"class"->"span4"},x___]->rightBarNew;
Export[htmlFiles[[i]],htmlFilesToExport[[2]],"XML"],{i,Length@htmlFilesImported}];

]


(* ::Subsection:: *)
(*DeletePost*)


(* ::Text:: *)
(*\:8fd9\:91cc\:4e3b\:8981\:662f\:5220\:9664nb\:6587\:4ef6\:ff0c\:5047\:8bbe\:4f60\:6709\:4e00\:7bc7nb\:6587\:4ef6\:662f\:81ea\:5df1\:5199\:7684\:ff0c\:5728\:522b\:7684\:76ee\:5f55[\:5efa\:8bae]\:ff0c\:7136\:540e\:53d1\:5e03\:540e\:ff0c\:4f1a\:81ea\:52a8\:751f\:6210\:4e00\:4e2a\:ff0c\:4fdd\:5b58\:5230Notebooks\:76ee\:5f55\:ff0c\:6b64\:65f6\:ff0c\:5047\:8bbe\:751f\:6210\:7684Notebook\:4f60\:7a81\:7136\:6682\:65f6\:4e0d\:60f3\:53d1\:5e03\:4e86\:ff0c\:6216\:60f3\:5220\:9664[\:6bd4\:5982\:5206\:7c7b\:5199\:9519\:4e86]\:ff0c\:5219\:53ef\:4ee5\:5220\:9664\:6b64nb\:6587\:4ef6\:ff0c\:4e0d\:7528\:53bb\:6253\:5f00Notebooks\:76ee\:5f55\:91cc\:627e\:4e86\:3002*)


nbBlogDelete[x0_]:=Module[{x=x0,files},
files=Select[FileNames["*.nb",dirNotebook[AL],\[Infinity]],StringContainsQ[#,ToString@x]&];
DeleteFile@files;
cleanSite[];
]


(* ::Section:: *)
(*\:4e3b\:51fd\:6570/\:65e5\:5fd7\:8f6c\:6362\:51fd\:6570*)


(* ::Subsection:: *)
(*Vars/Options*)


Options[nb2Post]={"class"->"Unclassified/","force"->False};


(* ::Subsection:: *)
(*main*)


nb2Post[nb0_,OptionsPattern[]]:=Module[{nb},

If[Head[nb0]===NotebookObject,nb=NotebookRead[Cells[nb0]],Abort[]];

recentPostUpdate[];
categoryUpdate[];
indexUpdate[];
cleanSite[];
refreshSite[];

nbExpressionNew=DeleteCases[nb,Cell[BoxData[RowBox[{"nb2Post",___}]],___],\[Infinity]];
(*\[Equal]=>*)
var[Force]=OptionValue["force"];
var[Category]=OptionValue["class"];var[Title]="noTitle";varXML[Title]="Untitled";titleList=Select[nb,#[[2]]=="Title"&];If[titleList=={},Null,var[Title]=titleList[[1,1]];varXML[Title]=titleList[[1,1]]];
(*\:672c\:5730\:4f7f\:7528\:6709\:7c7b\:76ee\:540d\:4f5c\:4e3a\:76ee\:5f55\:540d\:ff0c\:53d1\:5e03\:7248\:4f7f\:7528\:4e00\:7ea7\:76ee\:5f55*)
(*<\:7b14\:8bb0\:672c\:4fdd\:5b58>*)
fileNotebook[AL]=fileNameJoinUnix[{dirNotebook[AL],var[Category],var[Title]<>".nb"}];
dirNotebookInstance[AL]=pathFormatUnix@FileNameDrop@fileNotebook[AL];
If[!DirectoryQ[dirNotebookInstance[AL]],CreateDirectory[dirNotebookInstance[AL]]];
If[var[Force],Export[fileNotebook[AL],nbExpressionNew],If[FileExistsQ[fileNotebook[AL]],res=DialogInput[DialogNotebook[{TextCell["\:540c\:540d\:7684\:65e5\:5fd7\:5df2\:7ecf\:5b58\:5728\:ff0c\:53ef\:80fd\:6709\:51b2\:7a81"],Button["Proceed/\:7ee7\:7eed",DialogReturn[1]],Button["Abort/\:653e\:5f03/\:56de\:53bb\:4fee\:6539\:6587\:4ef6\:540d",DialogReturn[2]]}]];
If[res==1,Export[fileNotebook[AL],nbExpressionNew],Abort[]],Export[fileNotebook[AL],nbExpressionNew]]];

(*===\:9996\:6b21\:8f93\:51fa\:7684Working\:76ee\:5f55===*)
dirBlogWorking[AL]=fileNameJoinUnix[{dirBlogHome[AL],"Working","Working"}];(*\:9996\:6b21Export[HTML]\:7684\:5de5\:4f5c\:76ee\:5f55*)
fileHTMLWorking[AL]=fileNameJoinUnix[{dirBlogWorking[AL],var[Category],var[Title],var[Title]<>".html"}];(*\:9996\:6b21Export[HTML]\:7684\:6587\:4ef6\:540d*)
dirHTMLWorking[AL]=fileNameJoinUnix[{FileNameDrop[fileHTMLWorking[AL],OperatingSystem->"Unix"]}];(*\:9996\:6b21Export[HTML]\:7684HTMLFiles\:7684\:8def\:5f84*)
dirHTMLWorking[Relative]=FileNameDrop[dirHTMLWorking[AL],Length[FileNameSplit@dirBlogHome[AL]],OperatingSystem->"Unix"];

(*\:7528\:4e8e\:672c\:5730\:9884\:89c8\:7684\:76ee\:5f55\:ff0c\:6709\:5e03\:5c40\:548c\:6837\:5f0f*)
dirBlogSite[AL]=fileNameJoinUnix[{dirBlogHome[AL],"Site"}];(*\:7528\:4e8e\:672c\:5730\:9884\:89c8\:7684\:7ad9\:70b9\:6839\:76ee\:5f55*)
dirHTMLSite[AL]=fileNameJoinUnix[{dirBlogSite[AL],var[Category],var[Title]}];(*\:7528\:4e8e\:672c\:5730\:9884\:89c8\:7684HTML\:6587\:4ef6\:540d*)
fileHTMLSite[AL]=fileNameJoinUnix[{dirHTMLSite[AL],var[Title]<>".html"}];(*\:7528\:4e8e\:672c\:5730\:9884\:89c8\:7684HTML\:6587\:4ef6\:540d*)

(*\:53d1\:5e03\:7684\:76ee\:5f55*)
dirHTMLPush[AL]=fileNameJoinUnix[{dirBlogPush[AL],var[Title]}];
fileHTMLPush[AL]=fileNameJoinUnix[{dirHTMLPush[AL],var[Title]<>".html"}];
fileHTMLPushed[AL]=fileNameJoinUnix[{dirHTMLPush[AL],"/index.html"}];

links={"<link href=\"assets/themes/twitter/css/1.4.0/bootstrap.css\" rel=\"stylesheet\">","<link href=\"assets/themes/twitter/css/style.css?body=1\" rel=\"stylesheet\" type=\"text/css\" media=\"all\">"};
headElements={"<title>"<>varXML[Title]<>"</title>","<meta name=\"author\" content=\"HyperGroups\">"}~Join~links;

(*\:9996\:6b21\:8f93\:51faHTML\:6587\:4ef6*)
reCreateDirectoryForce[dirHTMLWorking[AL]];


Export[fileHTMLWorking[AL],nbExpressionNew,"MathOutput"->"PNG",CharacterEncoding->"UTF-8","CSS"->Automatic,"GraphicsOutput"->"PNG","HeadElements"->headElements]//Quiet;

xmlObj=Import[fileHTMLWorking[AL],"XMLObject"];

imgResize[x0_,width0__:700]:=Module[{x=x0,widthOld,heightOld,heightNew,widthNew,widthFixed=width0,imgFile},
imgFile="src"/.x;

{widthOld,heightOld}=ImageDimensions[Import[imgFile]];

If[widthOld>widthFixed,widthNew=widthFixed,widthNew=widthOld];heightNew=(widthNew/widthOld)heightOld//N//Round;
ReplaceAll[x,{("width"->w_):>("width"->ToString[widthNew]),("height"->h_):>("height"->ToString[heightNew])}]
];

path2Site[x_]:=StringReplace[x,{StartOfString~~"HTMLFiles/":>(dirHTMLWorking[AL]<>"/HTMLFiles/"),StartOfString~~"assets/":>(dirBlogHome[AL]<>"/assets/"),t:(StartOfString~~"/MBlogSimple"~~rest__)/;(!StringContainsQ[t,"Site"]):>(dirBlogSite[AL]<>rest),t:(StartOfString~~"/MBlogSimple/Site"~~rest__):>(dirBlogSite[AL]<>rest)}];

path2Push[x_]:=StringReplace[x,{dirHTMLWorking[AL]<>"/":>"",t:(dirBlogSite[AL]~~rest__):>"/MBlogSimple"<>rest,t:(dirBlogHome[AL]~~rest__)/;!StringContainsQ[t,{"Site","assets"}]:>"/MBlogSimple/"<>FileBaseName[rest],t:(dirBlogHome[AL]~~rest__)/;StringContainsQ[t,{"assets"}]:>"/MBlogSimple"<>rest}];
(*\:76ee\:524d\:53d1\:5e03\:7248\:91c7\:7528\:7684\:94fe\:63a5\:89c4\:5219\:6b64\:5904\:662f\:9879\:76ee\:8def\:5f84+\:6587\:4ef6\:540d*)

nameList={"topBar","blogRoll","category","recentPost","rightBarTemplate"};
dirLayout[AL]=fileNameJoinUnix[{dirBlogHome[AL],"Layout"}];
layoutFiles=pathFormatUnix@FileNames["*.xml",dirLayout[AL]];
fileNameTrim=StringTrim[FileBaseName[#],{"_Template","_Final"}]&;
templateFiles=Select[layoutFiles,StringContainsQ[#,"_Template"]&];
finalFiles=Select[layoutFiles,StringContainsQ[#,"_Final"]&];
otherFiles=Complement[layoutFiles,templateFiles,finalFiles];
(*\:53ef\:6539\:8fdb\:4e3atype\:6807\:8bb0*)
(*_Final\:6807\:8bc6\:7684\:4e3a\:56fa\:5b9a\:7684\:5185\:5bb9\:ff0c\:5982\:9876\:680f\:6837\:5f0f\:3002
Template\:6807\:8bc6\:7684\:4e3a\:751f\:6210_Final\:7684\:6a21\:677f\:6587\:4ef6\:ff0c\:6b64\:5904\:6f14\:793a\:7684\:662frightBar\:3002
\:5176\:4ed6\:4e3a\:751f\:6210\:7684\:6a21\:677f\:6587\:4ef6\:7684Final\:6587\:4ef6\:7684\:7d20\:6750\:6765\:6e90*)
varsString=fileNameTrim[#]&/@layoutFiles;
vars=ToExpression[fileNameTrim[#]]&/@layoutFiles;
import=Import[#,"XMLElement"]&;
Clear/@varsString;
ReleaseHold[Thread[Hold[Set][ToExpression[fileNameTrim[#]],import[#][[1]]]&/@layoutFiles]];
ToExpression["rightBar"];ToExpression["topBar"];(*\:7701\:7565\:6b64\:53e5\:ff0c\:4f1a\:6709\:4e00\:4e2a\:95ee\:9898\:ff0c\:56e0\:4e3a\:4e0a\:53e5\:7684\:7528\:6cd5\:5bfc\:81f4\:8fd9\:51e0\:4e2a\:53d8\:91cf\:4e3aGlobal`\:7684*)
rightBarNew=Global`rightBar/.XMLElement["insert",{"class"->"ToBeInsert"},{x__}]:>(ToExpression[x]);
topBarNew=Global`topBar;
exportXML[fileRecentPost[AL],rightBarNew];

(*Begin<===\[Equal]\:7528\:4e8e\:8def\:5f84\:66f4\:65b0*)
rules1={XMLElement["body",{},y___]:>XMLElement["body",{},{topBarNew,XMLElement["div",{"class"->"container"},{XMLElement["div",{"class"->"content"},{XMLElement["div",{"class"->"row"},{XMLElement["div",{"class"->"span14"},y],rightBarNew}]}]}]}],XMLElement["title",{},{"\n  Untitled\n "}]:>Sequence[](*,rules2\:4e3a\:4ec0\:4e48\:653e\:5728\:8fd9\:91cc\:5c31\:5931\:6548\:ff1f*)};

rules2Site={XMLElement["img",{"src"->path__,f__},{}]:>XMLElement["img",{"src"->path2Site[path],f},{}],XMLElement["a",{"href"->path__},{f__}]:>XMLElement["a",{"href"->path2Site[path]},{f}],XMLElement["a",{"class"->"brand","href"->path__},{"Hyper-Space"}]:>XMLElement["a",{"class"->"brand","href"->path2Site[path]},{"Hyper-Space"}]};
(*\:56fe\:7247\:7684\:8def\:5f84\:66f4\:65b0\:4e3a\:7edd\:5bf9\:8def\:5f84\:ff0c\:4f7f\:7528Working\:76ee\:5f55\:7684\:8def\:5f84\:ff0c\:7701\:5f97Copy\:4e00\:4efdHTMLFiles\:ff0c\:540c\:65f6\:65b9\:4fbf\:4e0b\:4e00\:5c42\:81ea\:52a8\:8c03\:6574\:5927\:5c0f\:524d\:ff0c\:80fd\:8f93\:5165\:56fe\:7247,
a\:6807\:7b7e\:7684\:8def\:5f84\:66f4\:65b0\:ff0c\:6bd4\:5982topBar\:4e2d\:7684*)

rules3Site={XMLElement["img",x___,{}]:>XMLElement["img",imgResize[x,700],{}]
,XMLElement["link",{"href"->path__,f__},{}]:>XMLElement["link",{"href"->path2Site[path],f},{}]};
(*\:56fe\:7247\:7684\:5927\:5c0f\:66f4\:65b0+css\:8def\:5f84\:66f4\:65b0*)

rules2Push={(*XMLElement["img",x___,{}]\[RuleDelayed]XMLElement["img",imgResize[x,700],{}]
,*)XMLElement["link",{"href"->path__,f__},{}]:>XMLElement["link",{"href"->path2Push[path],f},{}]};
(*css\:8def\:5f84\:66f4\:65b0*)

rules3Push={XMLElement["img",{"src"->path__,f__},{}]:>XMLElement["img",{"src"->path2Push[path],f},{}],(*\:4fa7\:8fb9\:680f\:7684\:8def\:5f84\:66f4\:65b0*)XMLElement["a",{"href"->x__},y___]:>XMLElement["a",{"href"->path2Push(*RecentPost*)[x]},y],XMLElement["a",{"class"->"brand","href"->path__},{"Hyper-Space"}]:>XMLElement["a",{"class"->"brand","href"->path2Push[path]},{"Hyper-Space"}]};
(*\:56fe\:7247\:7684\:8def\:5f84\:66f4\:65b0\:ff0c\:4f7f\:7528\:76f8\:5bf9\:8def\:5f84\:ff0c\:7701\:5f97Copy\:4e00\:4efdHTMLFiles*)

(*End\:8def\:5f84\:66f4\:65b0===>*)

xmlObjSite=xmlObj/.rules1/.rules2Site/.rules3Site;
(*[Site\:76ee\:5f55]*)

xmlObjPush=xmlObjSite/.rules2Push/.rules3Push;
(*[Push\:76ee\:5f55\:ff0c\:540c\:6b65\:5230GitHub\:ff0c\:5982\:672c\:5730\:7684GitHub/MBlogSimple\:76ee\:5f55]*)

(*\:8f93\:51fa\:66f4\:65b0\:540e\:7684HTML\:6587\:4ef6\:5230Site\:76ee\:5f55*)
reCreateDirectoryForce[dirHTMLSite[AL]];

exportXML[fileHTMLSite[AL],xmlObjSite];

(*\:8f93\:51fa\:66f4\:65b0\:540e\:7684HTML\:6587\:4ef6\:5230Push\:76ee\:5f55GitHub/MBlogSimple*)
copyDirectoryForce[dirHTMLWorking[AL],dirHTMLPush[AL]];

exportXML[fileHTMLPushed[AL],xmlObjPush];

If[FileExistsQ[fileHTMLPush[AL]],DeleteFile[fileHTMLPush[AL]]];

refreshSite[];

]


(* ::Section::Closed:: *)
(*GitPush*)


(* ::Text:: *)
(*\:4f7f\:7528Mathematica\:51fd\:6570\:8fd0\:884cGitShell\:547d\:4ee4\:6a21\:5757*)


(* ::Section::Closed:: *)
(*End*)


End[]


EndPackage[ ]
