# Diamond Point

{ex}
(ns diamond-point
  (:require [hiccup.core :refer [h html]]
            [stylo.draw :refer :all]))

(load-file "fabric.clj")
(use 'stylo.fabric)

(load-file "work/diamond-point-figures.clj")

(html (list
       fabric-styles
       [:style "
table{width:auto;}
th, td{vertical-align:top;padding:5px;border: 1px solid #ddd;}
table ul {list-style-type:none;padding-left:4px;margin:0}
table p {margin:0}
table li:before {content:'▢ ';}
@media print {.pagebreak {page-break-before:always;}}
.figure{padding-left:7px;}
.figure p{font-size:smaller;font-style:italic;}
.trim{fill:#fff;stroke:#fff !important;}

.A{fill:rgb(196, 213, 244);}
.B{fill:rgb(120, 137, 162);}
.C{fill:rgb(225, 223, 234);}
.D{fill:rgb(225, 223, 234);}
.E{fill:rgb(225, 223, 234);}
.F{fill:rgb(225, 223, 234);}
.G{fill:rgb(225, 223, 234);}
.H{fill:rgb(225, 223, 234);}
.I{fill:rgb(225, 223, 234);}
.J{fill:rgb( 65,  95, 135);}
.K{fill:rgb( 67, 104, 150);}
.L{fill:rgb(235, 235, 235);}
.berry-b{fill:rgba(140, 154, 168);}
"]))
{ex}

Finished Size: 26 1/2" x 46 1/2"

I encourage you to take a few minutes and read through all the instructions before beginning.

## A few notes:
- All seam allowances are 1/4". 
- WOF means width of fabric and assumes 42" unwashed.
- RST means right sides of the fabric together.
- The markings on the templates are very important in the construction of this pattern. Accurate transfer of the dots to the wrong side of the fabric pieces is imperative.

# Fabric Requirements
These are **exact** calculations. Add extra yardage for your personal comfort level.

{tb}
|swatch|Fabric|Description|Requirements|
|{ex}
(svg [42 42 20] (sq 2 "A"))
{ex}|**A**|*Navy flowers with cream background*|11" WOF|
|{ex}
(svg [42 42 20] (sq 2 "B"))
{ex}|**B**|*Light blue floral*|22" WOF&ast;|
|{ex}
(svg [42 42 20] (sq 2 "C"))
{ex}|**C, D, E, F, G, H, I**|*Background*|16 1/2" WOF|
|{ex}
(svg [42 42 20] (sq 2 "J"))
{ex}|**J**|*Navy*|1" x 16" piece|
|{ex}
(svg [42 42 20] (sq 2 "K"))
{ex}|**K**|*Border and Binding*|15" WOF|
|{ex}
(svg [42 42 20] (sq 2 "L"))
{ex}|**L**|*Backing*|32" x 53" piece|
{tb}

&ast; This sample features three different light blue florals.  Two pieces were fussy cut out of one fat quarter.  Four were cut out of another floral requiring 11" x 22". The remaining four were cut out of a third light blue floral requiring 11" x 22".

# Cutting Instructions
Use the templates at the end of this document to cut the A, B, C, D, E, F, G, H pieces. Cut the following numbers of each, paying attention to those that must be reversed. Place templates adhering to the grain line markings.

{ex}
cutting-instructions
{ex}

Additionally, follow cutting instructions in the table below.

{tb}
|Swatch|Fabric|Cutting|
|{ex}
(svg [42 42 20] (sq 2 "H"))
{ex}|**H**|- 2 7/8” squares (2), cut in half diagonally|
|{ex}
(svg [42 42 20] (sq 2 "I"))
{ex}|**I**|- 2 5/8” x 18 1/4” (2)|
|{ex}
(svg [42 42 20] (sq 2 "J"))
{ex}|**J**|- 1” x 1” (16)|
|{ex}
(svg [42 42 20] (sq 2 "K"))
{ex}|**K**|- 2 1/2” x 18 1/4” (2)
- 2 1/2” x 18 1/2” (2)
- 2 1/2” x 20 3/4” (2)
- 2 1/2” WOF (3)|
{tb}

{ex}
(bb-method "J" "berry-b" "C" "C" "C" "C")
{ex}

## Step One
On the wrong side, mark all the dots on each template piece. If the template is out of your printer paper, it is easy to pierce the dot with a friction pen through to the fabric. The dot is easily transferred and in the direct spot necessary for accurate piecing. Taking your time with this step with give you a beautiful result in the finished project.

Lay out the four **A** pieces alternating with four **B** pieces according to *Fig. 1*.

{ex}
fig-01
{ex}

RST sew each of the long sides together starting with matching the dots of **A** to the dots of **B**. Sew from dot to dot. Backstitch at each dot. Press to the one side. As you add another piece, continue using the same method and press in the same direction to create a star pattern on the *wrong* at the very centre. This will help to reduce the bulk. See *Fig. 2*.

{ex}
fig-02
{ex}

## Step Two
Lay out all four **C** pieces according to *Fig. 3*.

{ex}
fig-03
{ex}

Attach a **C** piece to **A**. Sew the straight side first, from dot to the end. Next, sew the angle from dot to dot. Press away from the background. Sew the opposite **C** (reversed piece) to the other side, along the straight side first and then the angle. On this angle seam, sew from the the dot towards and through the centre dot to the end.  Match dot to dot at the seam. See *Fig. 4*.

{ex}
fig-04
{ex}

# Step Three
Repeat *Step Two* with all four **D** pieces using the layout in *Fig. 5*. You have completed the Centre Unit. It should measure 18 1/4” x 18 1/4”.

{ex}
fig-05
{ex}

# Step Four
Lay out the three **B** pieces designated for the side units and one **E** piece. See *Fig. 6*.

{ex}
fig-06
{ex}

Sew along the short sides, matching all dots. Press to the centre **B**. Attach **E**. Start at the centre dot and sew to the ends. Press to **B**.

# Step Five
Attach one **F** piece to **B** starting at the dot (remember to backstitch) and sewing to the end. See *Fig. 7*.  Repeat for the reverse **F** piece. Press to B.

{ex}
fig-07
{ex}

# Step Six
Attach **G** to **B** starting at the centre point and sew to the end. Matching the dots here is very important, especially since this is a bias seam and there may be some stretching if not mindful of these marks. Press to **B**. You now have a completed the first Side Unit.

Repeat Steps Four through Six to finish the second Side Unit.

{ex}
fig-07b
{ex}

# Step Seven
Lay out the Centre Unit, the two Side Units, the four **H** pieces, and two **I** pieces according to *Fig. 8*. Apply the Blueberry Method to the designated areas. Sew the **H** pieces to the sides of the Side Unit. Press to **H**. <u>Do not attach the Side Units yet.</u>

{ex}
fig-08
{ex}

# Step Eight - Borders
Take the 2 1/2” x 18 1/4” **K** pieces and attach one on two opposite sides of the Centre Unit. Press towards **K**.

Take one 2 1/2” x 18 1/2” piece and attach to one straight side of a Side Unit, matching the straight end to a straight end, and extending the other end 2” past the point. Pin in position to avoid stretching. See *Fig. 9*.

{ex}
fig-09 
{ex}

Press towards **K** and trim on the angle. See *Fig. 10*.

{ex}
fig-10 
{ex}

Sew the 2 1/2” x 20 3/4” **K** piece on the other side by repeating this application. Press towards the border. Complete the second Side Unit as described above.

Attach the Side Units to each end of the Centre Unit/H/K Unit, completing the Blueberries and matching the borders. Press to the Centre Unit/H/K Unit. The border will not match the outside edge. Simply trim the extra triangle to create a continuous and even edge. See *Fig. 11*.

{ex}
fig-11 
{ex}

# Finishing
To prepare the quilt for hand quilting, press the backing open and lay flat onto a surface large enough for the quilt. Taping this piece down is helpful to ensure the fabric stays very straight and prevents bubbling.

Lay out batting over the backing.

After a final pressing, position the quilt top onto the batting.

Hand baste through all the layers in a grid pattern, preferably a hand width apart for each row. This ensures the layers stay together without shifting when you start to quilt.

Hand quilt according to designs and patterns you desire.

# Binding
Sew the 3 remaining **K** strips together, end to end. Trim to 1/4” and press one way. Press the full length of the strip, wrong sides together and raw edge to raw edge. Your long strip will now be 1 1/4” wide.

Sew the binding onto the front. Turn the binding around and towards the back.  Hand sew in place.

You have successfully completed your *Diamond Point* Quilt.

{ex}
fig-12
{ex}

I would love to see your finished project. You can tag me *@blueberryquiltco* on Instagram.

# Templates
Print the templates 1:1 scale. Use the 1" square to confirm that the prints are properly sized. Template G cannot fit on a US Letter sized paper, so two cutouts are provided, one shifted left, and the other shifted right. 

The recommended method for creating the template is to cut both template pieces and use the middle point of the triangle to align each piece and tape them to form the final correct template shape. At this point, you can trace the template's shape on a larger piece of paper or template plastic if you wish.

{ex}
[:div.pagebreak] 

(def seam-match
  (list
    (mv [0.275 1] (rot 90 [0 0] (sc 0.25 (label "seam match"))))
    (ln [0.2 0.2] [0.2 2.625])
    (sc 0.1 (arw [2 2] [-0.5 2]))
    (sc 0.1 (arw [2 26.25] [-0.5 26.25]))))

(def grain-line
  (list
    (mv [0.175 0.5] (rot 90 [0 0] (sc 0.25 (label "grain-line"))))
    (sc 0.1 (arw [0 7.5] [0 0]))
    (sc 0.1 (arw [0 7.5] [0 15]))))

(figure [900 1365 scale-1-to-1] ""
        scale-sq
        (mv [0.35 0.35] 
          template-a
          (mv [2.875 0.5] grain-line)))

[:div.pagebreak]


(figure [1100 1350 scale-1-to-1] ""
        scale-sq
        (mv [0.35 0.35] 
          template-b
          (mv [2.3 0.5] grain-line))
        (->> (list
               template-c
               seam-match)
             (rot -70 [0 0])
             (mv [3.75 8.5])))

[:div.pagebreak]

(figure [1100 1400 scale-1-to-1] ""
        scale-sq
        (->> (list 
               template-d
               (mv [2.75 0] (rot 90 [0 0] seam-match)))
             (rot -90 [0 0])
             (mv [0.3  9.75]))
        (->> (list
               template-e
               (mv [0.25 0.25] (rot -45 [0 0] grain-line)))
             (rot 0 [0 0])
             (mv [4 0.3]))
        (->> (list
               template-f
               (mv [6.25 0.375] (rot 0 [0 0] grain-line)))
             (rot 90 [0 0])
             (mv [6.5 3])))

[:div.pagebreak]
(figure [1100 1400 scale-1-to-1] ""
        scale-sq
        (->> (list
               template-g
               (mv [6.25 0.375] (rot 55 [0 0] grain-line)))
             (rot -90 [0 0])
             (mv [0.3 13.5]))
        (->> (list
               template-g
               (mv [6.25 0.375] (rot 55 [0 0] grain-line)))
             (rot -90 [0 0])
             (mv [4 9])))
{ex}

