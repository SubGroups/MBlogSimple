(* ::Package:: *)

BeginPackage["MBlogSimple`"]

MyMathematicaBlog::usage =
        "\:5c06\:7b14\:8bb0\:672c\:751f\:6210Github\:535a\:5ba2\:9875\:9762\:53ca\:4e0a\:4f20GitHub\:7b49";


nb2Index::usage="\:751f\:6210\:4e3b\:9875\:7684\:7f51\:9875";
nb2Post::usage="\:751f\:6210\:65e5\:5fd7\:7684\:7f51\:9875";


buildSite::usage="\:751f\:6210\:7ad9\:70b9\:7248\:548c\:975e\:7ad9\:70b9\:7248\:ff0c\:7528\:4e8e\:672c\:5730\:9884\:89c8";


nbBlogDelete::usage="\:628a\:76f8\:5173\:65e5\:5fd7\:5220\:9664";


Begin["`Private`"];


(* ::Section:: *)
(*\:5355\:5143\:8f6c\:5316\:51fd\:6570*)


(* ::Subsubsection::Closed:: *)
(*DisplayFormula\:91cc\:7684\:6587\:672c\:5355\:5143\:5148\:53d6\:51fa\:ff0c\:518d\:66ff\:6362\:3002*)


InlineFormulaCell2GitHub[cellExpression_]:=Module[{CellExpression=cellExpression,cellExpressionNew,str},
  (styleBoxes =styleBoxesGet@CellExpression;

styleBoxesRule[styleBoxes_]:=Module[{posAll,pos},(pos=(posAll=Position[styleBoxes[[1]],_String,Infinity]);
If[posAll==={},"SingleColorLeft"<>styleBoxes[[1]]<>"SingleColorRight",RowBox[{"BlockColorLeft{",styleBoxes[[1]],"}BlockColorRight"}]])];


styleBoxesColored=styleBoxesRule/@styleBoxes;


rulesStyleBoxes=Thread[Rule[styleBoxes,styleBoxesColored]]);

cellExpressionNew = CellExpression /. rulesStyleBoxes/."\[IndentingNewLine]"->"IndentNewLine";
temp=cellExpressionNew;

str=MyString`stringReplaceRepeated[StringCases[t=ExportString[cellExpressionNew, "Tex", "BoxRules"->{SubscriptBox[x,y]:>x<>"_"<>y},"ConversionRules" ->{ "DisplayFormulaNumbered" -> {"$$","\\tag{"<>ToString[DisplayFormulaCount]<>"}$$"},"
" -> {"$$","$$"}, "InlineFormula" -> {"", ""}}] , "\\begin{document}" ~~ xxx___ ~~ "\\end{document}" -> StringTrim@xxx][[1]], {Shortest["\\(\\(" ~~ yyy___ ~~ "\\)\\)"] -> "$" ~~ yyy ~~ "$", Shortest["\\(" ~~ yyy___ ~~ "\\)"] -> "$" ~~ yyy ~~ "$", " { }" -> " ", (*"\\\\" -> "\n\n",*) Shortest["\\begin{doublespace}" ~~ yyy___ ~~ "\\end{doublespace}"] -> StringTrim@yyy,Shortest["\\begin{equation}" ~~ yyy___ ~~ "\\end{equation}"] -> StringTrim@yyy, "\\noindent" -> "", "\\noindent$\\pmb" -> "$", "\\section*" -> "", Shortest["\\title{" ~~ x___ ~~ "}"] ~~ y___ ~~ EndOfString -> x(*, "\\\\\r\n" -> "\r\n"*)}];
s=str;

StringReplace[StringReplace[str,{Shortest["\\text{BlockColorLeft$\\{$}"~~x___~~"\\text{$\\}$BlockColorRight}"]:>"\\color{blue}{"<>x<>"}",
Shortest["\\text{SingleColorLeft$"~~x__~~"$SingleColorRight}"]:>"\\color{red}{"<>x<>"}",
Shortest["\\text{SingleColorLeft"~~x__~~"SingleColorRight}"]:>"\\color{red}{"<>x<>"}",
Shortest["\\text{BlockColorLeft}\\{"~~x__~~"\\}\\text{BlockColorRight}"]:>"\\color{Green}{"<>x<>"}",
"_{\\text{}}"->"","{}"->""
}],{"\n\n"->"","\r\n"->"","\\text{IndentNewLine}"->"\\\\&"(*,"{}"->*),"\\text{...}"->"\\cdots ","\\textit{$"~~x__~~"$}":>""<>x<>"",Shortest["\\$"~~x__~~"$]"]->x,"$$\\begin{align*}"->"\\begin{align*}","\\end{align*}$$"->"\\end{align*}",Shortest["\\text{"~~x__~~"}"]:>"\\text{ "<>x<>"}"}]//StringReplace[#,{"\\["->"$$","\\]"->"$$","&"->""}]&

]


(* ::Section:: *)
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


(* ::Section:: *)
(*Common*)


pathFormatUnix:=StringReplace[#1,"\\"->"/"]&;


fileNameJoinUnix:=FileNameJoin[#1,OperatingSystem->"Unix"]&;


(* ::Section:: *)
(*\:4e3b\:51fd\:6570*)


(* ::Subsection:: *)
(*Vars/Options*)


Options[nb2Post]={"class"->"Unclassified/"};


dirGitHub[AbsoluteLocal]=pathFormatUnix@fileNameJoinUnix@{$UserDocumentsDirectory,"GitHub"};


dirGitHubBlog[AbsoluteLocal]=pathFormatUnix@fileNameJoinUnix@{$UserDocumentsDirectory,"GitHub","MBlogSimple"};


dirGitHubBlog[Relative]="MBlogSimple";


dirGitHubBlogPush[AbsoluteLocal]=pathFormatUnix@fileNameJoinUnix@{$UserDocumentsDirectory,"GitHub","MBlogSimple"};
(*Push\:7684\:76ee\:5f55\:53ef\:4ee5\:8bbe\:7f6e\:6210\:4e0d\:540c\:7684\:ff0c\:56e0\:6b64\:6709\:8fd9\:91cc\:7684\:547d\:540d\:ff0c\:56e0\:4e3a\:6b64\:5904\:81ea\:52a8\:7248\:662f\:8bbe\:7f6e\:6210\:76f8\:540c\:7684\:76ee\:5f55\:ff0c\:56e0\:6b64Push\:6807\:8bb0\:7684\:6ca1\:6709\:663e\:793a\:51fa\:5dee\:5f02\:3002*)


(* ::Subsection:: *)
(*main*)


(* ::Input:: *)
(*nb2Post[nb0_:EvaluationNotebook[],OptionsPattern[]]:=Module[{nb=NotebookRead[Cells[nb0]]},*)
(**)
(*nbExpression=DeleteCases[DeleteCases[DeleteCases[nbExpressionRaw=nb,Cell[___,CellTags->"Ignore",___],\[Infinity]],Cell[___,"Tag",___],\[Infinity]],Cell[BoxData[RowBox[{"Notebook2GitHubPage",___}]],___],\[Infinity]];*)
(**)
(*FormulaCell2GitHub[xx_]:=StringReplace[InlineFormulaCell2GitHub[xx],{"<"->"&lt;",">"->"&gt;"}];*)
(**)
(*nbExpressionNew=nbExpression/.{BoxData[{x__}]->BoxData[RowBox[{x}]],"Code"->"Input",StyleBox[x__,"Title"]:>Cell[x,"InputInline"],StyleBox[x__,"Section"]:>Cell[x,"InlineSection"],StyleBox[x__,"Text"]:>" "<>FormulaCell2GitHub[Cell[x,"InlineFormula"]]};*)
(**)
(*(*\:683c\:5f0f\:5316\:4ee3\:7801\:7684\:9884\:5904\:7406===*)*)
(*headList={SuperscriptBox,SubscriptBox,GraphicsBox};*)
(*replace[box0_,list0_]:=Module[{box=box0,(*box\:8981BoxData[]\:7684\:6709\:4e00\:4e2a\:5934\:7684\:ff0c\:5426\:5219\:76f4\:63a5GraphicsBox\:63d0\:53d6\:4e0d\:51fa\:6765*)list=list0},cases=Cases[box,_?(MemberQ[list,Head[#1]]&),\[Infinity]];rules1=Thread[cases->(ToExpression["aAa"<>ToString[#1]]&)/@Range[Length[cases]]];rules2=Reverse/@rules1;box/. rules1];*)
(**)
(*inputFormat[x0_]:=Module[{x=x0},If[Head[x]===BoxData,y=x,y=BoxData[x]];ReplaceAll[FullCodeFormat@replace[y,headList][[1]],rules2]];*)
(**)
(*sectionList=Cases[nbExpressionNew,Cell[__,"Section",___],\[Infinity]][[All,1]];*)
(*inputList=Cases[nbExpressionNew,Cell[__,"Input",___],\[Infinity]][[All,1]];*)
(*inputListFormatted=BoxData[inputFormat[#]]&/@inputList;*)
(*nbExpressionFormatted=nbExpressionNew/.Thread[inputList->inputListFormatted];*)
(**)
(*category=OptionValue["class"];title="noTitle";titleList=Select[nb,#[[2]]=="Title"&];If[titleList=={},Null,title=titleList[[1,1]]];*)
(*(*\:672c\:5730\:4f7f\:7528\:6709\:7c7b\:76ee\:540d\:4f5c\:4e3a\:76ee\:5f55\:540d\:ff0c\:53d1\:5e03\:7248\:4f7f\:7528\:4e00\:7ea7\:76ee\:5f55*)*)
(**)
(*(*===\:9996\:6b21\:8f93\:51fa\:7684Working\:76ee\:5f55===*)*)
(*dirBlogWorking[AbsoluteLocal]=fileNameJoinUnix[{dirGitHubBlog[AbsoluteLocal],"Working"}];(*\:9996\:6b21Export[HTML]\:7684\:5de5\:4f5c\:76ee\:5f55*)*)
(*fileHTMLWorking[AbsoluteLocal]=fileNameJoinUnix[{dirBlogWorking[AbsoluteLocal],category,title,title<>".html"}];(*\:9996\:6b21Export[HTML]\:7684\:6587\:4ef6\:540d*)*)
(*dirHTMLWorking[AbsoluteLocal]=fileNameJoinUnix[{FileNameDrop[fileHTMLWorking[AbsoluteLocal],OperatingSystem->"Unix"]}];(*\:9996\:6b21Export[HTML]\:7684HTMLFiles\:7684\:8def\:5f84*)*)
(*dirHTMLWorking[Relative]=FileNameDrop[dirHTMLWorking[AbsoluteLocal],6,OperatingSystem->"Unix"];*)
(**)
(*(*\:7528\:4e8e\:672c\:5730\:9884\:89c8\:7684\:76ee\:5f55\:ff0c\:6709\:5e03\:5c40\:548c\:6837\:5f0f*)*)
(*dirBlogSite[AbsoluteLocal]=fileNameJoinUnix[{dirGitHubBlog[AbsoluteLocal],"Site"}];(*\:7528\:4e8e\:672c\:5730\:9884\:89c8\:7684\:7ad9\:70b9\:6839\:76ee\:5f55*)*)
(*dirHTMLSite[AbsoluteLocal]=fileNameJoinUnix[{dirBlogSite[AbsoluteLocal],category,title}];(*\:7528\:4e8e\:672c\:5730\:9884\:89c8\:7684HTML\:6587\:4ef6\:540d*)*)
(*fileHTMLSite[AbsoluteLocal]=fileNameJoinUnix[{dirHTMLSite[AbsoluteLocal],title<>".html"}];(*\:7528\:4e8e\:672c\:5730\:9884\:89c8\:7684HTML\:6587\:4ef6\:540d*)*)
(**)
(*(*\:53d1\:5e03\:7684\:76ee\:5f55*)*)
(*dirHTMLPush[AbsoluteLocal]=fileNameJoinUnix[{dirGitHubBlogPush[AbsoluteLocal],title}];*)
(*fileHTMLPush[AbsoluteLocal]=fileNameJoinUnix[{dirHTMLPush[AbsoluteLocal],title<>".html"}];*)
(*fileHTMLPushed[AbsoluteLocal]=fileNameJoinUnix[{dirHTMLPush[AbsoluteLocal],"/index.html"}];*)
(**)
(*links={"<link href=\"assets/themes/twitter/css/1.4.0/bootstrap.css\" rel=\"stylesheet\">","<link href=\"assets/themes/twitter/css/style.css?body=1\" rel=\"stylesheet\" type=\"text/css\" media=\"all\">"};*)
(*headElements={"<title>"<>nb[[1,1]]<>"</title>","<meta name=\"author\" content=\"HyperGroups\">"}~Join~links;*)
(**)
(*(*\:9996\:6b21\:8f93\:51faHTML\:6587\:4ef6*)*)
(*If[DirectoryQ@dirHTMLWorking[AbsoluteLocal],DeleteDirectory[dirHTMLWorking[AbsoluteLocal],DeleteContents->True];CreateDirectory@dirHTMLWorking[AbsoluteLocal];,CreateDirectory@dirHTMLWorking[AbsoluteLocal];];*)
(*(*If[FileExistsQ[dirHTML[AbsoluteLocal]],DeleteDirectory[dirHTML[AbsoluteLocal],DeleteContents\[Rule]True]];*)*)
(**)
(*(*Export[fileHTMLWorking[AbsoluteLocal],nb,CharacterEncoding->"UTF8","HeadElements"->headElements];*)*)
(*inputBox2String=First[FrontEndExecute[FrontEnd`ExportPacket[Cell[BoxData[#],"Input"],"InputText"]]]&;*)
(**)
(*Counter[Output]=0;Counter[Section]=1;Counter[Input]=1;*)
(*Export[fileHTMLWorking[AbsoluteLocal],nbExpressionNew,"ConversionRules"->{"Section"->{"<div id=\"section"<>ToString[(Counter[Section]++)]<>"\"><p class=\"Section\">",(*Counter[Section]++;*)#&,"</p></div>"},"InlineSection"->{"<span style=\"background-color:#99FF00;\">","</span>"},"Input"->{"<div><input class=\"flip-button\" id=\"[Counter[InputB]]Hide\" style=\"width:50px;\" value=\"In[[Counter[InputC]]]:=\">"<>"<div id=\"[Counter[InputA]]\""<>" style='display:none;'>"<>"\n{% raw %}<pre class=\"prettyprint lang-mma\"><pre class=\"Input\" style=\"white-pace:nowrap; padding-left:50px; background-color:#FF359A;\"><code>",(Counter[Input]++;StringReplace[temp=(*strFormat@*)inputBox2String[#],{"<"->"&lt;",">"->"&gt;"}])&,"</code></pre></pre>{% endraw %}</div>"<>"<br><p class=\"Input\"><img height=260 width=425 src=\"InputImage[Counter[Input]]"<>"\"></p></div>\n"},"InputInline"->{"\n<span class=\"InlineCode\" style=\"background-color:#99FF00;\">",StringReplace[inputBox2String[#],{"<"->"&lt;",">"->"&gt;"}]&,"</span>\n"},"CodeDisplay"->{"\n<pre class=\"prettyprint lang-mma\"><pre class=\"CodeDisplay\" style=\"white-pace:nowrap; overflow: auto;\"><code>","</code></pre></pre>\n"},"HTML"->{"\n<HTML class=\"Input\">",StringReplace[#,{"&lt;"->"<","&gt;"->">"}]&,"</HTML>\n"},"InlineFormula"->{"",StringReplace[FormulaCell2GitHub[Cell[BoxData[#],"InlineFormula"]],{"<"->"&lt;",">"->"&gt;"}]&,""},"DisplayFormula"->{"<p class=\"DisplayFormula\">$",FormulaCell2GitHub[Cell[BoxData[#],"InlineFormula"]]&,"$"},"DisplayFormulaNumbered"->{"$",FormulaCell2GitHub[Cell[BoxData[#],"InlineFormula"]]&,"$"},"NumberedEquation"->{"$",FormulaCell2GitHub[Cell[BoxData[#],"InlineFormula"]]&,"$"}},"MathOutput"->"PNG",CharacterEncoding->"UTF-8","CSS"->Automatic,"GraphicsOutput"->"PNG"]//Quiet;*)
(**)
(**)
(*xmlObj=Import[fileHTMLWorking[AbsoluteLocal],"XMLObject"];*)
(**)
(*imgResize[x0_,width0__:700]:=Module[{x=x0,widthOld,heightOld,heightNew,widthNew,widthFixed=width0,imgFile},*)
(*imgFile="src"/.x;*)
(**)
(*{widthOld,heightOld}=ImageDimensions[Import[imgFile]];*)
(**)
(*If[widthOld>widthFixed,widthNew=widthFixed,widthNew=widthOld];heightNew=(widthNew/widthOld)heightOld//N//Round;*)
(*ReplaceAll[x,{("width"->w_):>("width"->ToString[widthNew]),("height"->h_):>("height"->ToString[heightNew])}]*)
(*];*)
(**)
(*path2Site[x_]:=StringReplace[x,{StartOfString~~"HTMLFiles/":>(dirHTMLWorking[AbsoluteLocal]<>"/HTMLFiles/"),StartOfString~~"assets/":>(dirGitHubBlog[AbsoluteLocal]<>"/assets/")}];*)
(**)
(*path2Push[x_]:=StringReplace[x,{StartOfString~~dirHTMLWorking[AbsoluteLocal]<>"/":>"",StartOfString~~dirGitHubBlog[AbsoluteLocal]:>("")}];*)
(**)
(*(*Begin<===\[Equal]\:7528\:4e8e\:8def\:5f84\:66f4\:65b0*)*)
(*rules1={XMLElement["body",{},y___]:>XMLElement["body",{},topBar~Join~{XMLElement["div",{"class"->"container"},{XMLElement["div",{"class"->"content"},{XMLElement["div",{"class"->"row"},{XMLElement["div",{"class"->"span14"},y]}~Join~rightBar]}]}]}],XMLElement["title",{},{"\n  Untitled\n "}]:>Sequence[](*,rules2\:4e3a\:4ec0\:4e48\:653e\:5728\:8fd9\:91cc\:5c31\:5931\:6548\:ff1f*)};*)
(**)
(*rules2Site={XMLElement["img",{"src"->path__,f__},{}]:>XMLElement["img",{"src"->path2Site[path],f},{}]};*)
(*(*\:56fe\:7247\:7684\:8def\:5f84\:66f4\:65b0\:4e3a\:7edd\:5bf9\:8def\:5f84\:ff0c\:4f7f\:7528Working\:76ee\:5f55\:7684\:8def\:5f84\:ff0c\:7701\:5f97Copy\:4e00\:4efdHTMLFiles\:ff0c\:540c\:65f6\:65b9\:4fbf\:4e0b\:4e00\:5c42\:81ea\:52a8\:8c03\:6574\:5927\:5c0f\:524d\:ff0c\:80fd\:8f93\:5165\:56fe\:7247*)*)
(**)
(*rules3Site={XMLElement["img",x___,{}]:>XMLElement["img",imgResize[x,700],{}]*)
(*,XMLElement["link",{"href"->path__,f__},{}]:>XMLElement["link",{"href"->path2Site[path],f},{}]};*)
(*(*\:56fe\:7247\:7684\:5927\:5c0f\:66f4\:65b0+css\:8def\:5f84\:66f4\:65b0*)*)
(**)
(*rules2Push={(*XMLElement["img",x___,{}]\[RuleDelayed]XMLElement["img",imgResize[x,700],{}]*)
(*,*)XMLElement["link",{"href"->path__,f__},{}]:>XMLElement["link",{"href"->path2Push[path],f},{}]};*)
(*(*css\:8def\:5f84\:66f4\:65b0*)*)
(**)
(*rules3Push={XMLElement["img",{"src"->path__,f__},{}]:>XMLElement["img",{"src"->path2Push[path],f},{}]};*)
(*(*\:56fe\:7247\:7684\:8def\:5f84\:66f4\:65b0\:ff0c\:4f7f\:7528\:76f8\:5bf9\:8def\:5f84\:ff0c\:7701\:5f97Copy\:4e00\:4efdHTMLFiles*)*)
(**)
(*(*End\:8def\:5f84\:66f4\:65b0===>*)*)
(**)
(*layoutFiles={"C:\\Users\\HyperGroups\\Documents\\GitHub\\MathematicaBlogLocal\\Layout\\blogRoll.xml","C:\\Users\\HyperGroups\\Documents\\GitHub\\MathematicaBlogLocal\\Layout\\catList.xml","C:\\Users\\HyperGroups\\Documents\\GitHub\\MathematicaBlogLocal\\Layout\\recentPost.xml","C:\\Users\\HyperGroups\\Documents\\GitHub\\MathematicaBlogLocal\\Layout\\rightBar_Template.xml","C:\\Users\\HyperGroups\\Documents\\GitHub\\MathematicaBlogLocal\\Layout\\topBar.xml"};*)
(**)
(*import=Import[#,"XMLElement"]&;*)
(*rightBarTemplate=import@layoutFiles[[4]];*)
(*{topBar,blogRollBar,categoryBar,recentPostBar}=import/@layoutFiles[[{-1,1,2,3}]];*)
(*rightBar=rightBarTemplate/.{"CategoryContent"->categoryBar[[1]],"BlogRollContent"->blogRollBar[[1]],"RecentPostContent"->recentPostBar[[1]]};*)
(**)
(**)
(*xmlObjSite=xmlObj/.rules1/.rules2Site/.rules3Site;*)
(*(*[Site\:76ee\:5f55]*)*)
(**)
(*xmlObjPush=xmlObjSite/.rules2Push/.rules3Push;*)
(*(*[Push\:76ee\:5f55\:ff0c\:540c\:6b65\:5230GitHub\:ff0c\:5982\:672c\:5730\:7684GitHub/mathemtaica\:76ee\:5f55]*)*)
(**)
(**)
(*(*\:8f93\:51fa\:66f4\:65b0\:540e\:7684HTML\:6587\:4ef6\:5230Site\:76ee\:5f55*)*)
(*If[DirectoryQ@dirHTMLSite[AbsoluteLocal],DeleteDirectory[dirHTMLSite[AbsoluteLocal],DeleteContents->True];CreateDirectory@dirHTMLSite[AbsoluteLocal];,CreateDirectory@dirHTMLSite[AbsoluteLocal]];*)
(**)
(*Export[fileHTMLSite[AbsoluteLocal],xmlObjSite,"XML","AttributeQuoting"->"\"",CharacterEncoding->"UTF8"];*)
(**)
(*(*\:8f93\:51fa\:66f4\:65b0\:540e\:7684HTML\:6587\:4ef6\:5230Push\:76ee\:5f55GitHub/mathematica*)*)
(*If[DirectoryQ@dirHTMLPush[AbsoluteLocal],DeleteDirectory[dirHTMLPush[AbsoluteLocal],DeleteContents->True];CreateDirectory@dirHTMLPush[AbsoluteLocal];,CreateDirectory@dirHTMLPush[AbsoluteLocal]];*)
(**)
(*If[DirectoryQ@dirHTMLPush[AbsoluteLocal],*)
(*DeleteDirectory[dirHTMLPush[AbsoluteLocal],DeleteContents->True];CopyFile[dirHTMLWorking[AbsoluteLocal],dirHTMLPush[AbsoluteLocal]],CopyFile[dirHTMLWorking[AbsoluteLocal],dirHTMLPush[AbsoluteLocal]]];*)
(**)
(*Export[fileHTMLPushed[AbsoluteLocal],xmlObjPush,"XML","AttributeQuoting"->"\"",CharacterEncoding->"UTF8"];*)
(*If[FileExistsQ[fileHTMLPush[AbsoluteLocal]],DeleteFile[fileHTMLPush[AbsoluteLocal]]]*)
(**)
(*]*)


(* ::Section:: *)
(*Delete*)


(* ::Text:: *)
(*\:5220\:9664\:4e00\:4e9b\:65e7\:7684\:65e5\:5fd7*)


nbBlogDelete[x0_]:=Module[{x=x0,dir,dir1,files,dirs,dirsToDelete},
dir="C:\\Users\\HyperGroups\\Documents\\GitHub\\hypergroups.github.com\\_posts";
dir1="C:\\Users\\HyperGroups\\Documents\\GitHub\\hypergroups.github.com\\HTMLFiles";
dirs={dir,dir1};
files=Select[FileNames["*.html",dirs,\[Infinity]],StringContainsQ[#,ToString@x]&];
dirsToDelete=Select[FileNames["*",dirs,\[Infinity]],StringContainsQ[#,ToString@x]&&DirectoryQ[#]&];
DeleteFile@files;
DeleteDirectory[dirsToDelete,DeleteContents->True];

]


(* ::Section:: *)
(*GitPush*)


(* ::Section:: *)
(*End*)


End[]


EndPackage[ ]
