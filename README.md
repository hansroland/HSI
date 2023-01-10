# HSI Halfspace Intersection: Calculate and draw convex polytopes from half-space equations

<img src="./images/hsiexamples.svg"/>

# Basics

## Half-spaces

In the euclidian space R<sup>n</sup> a half-space is an (n-1) dimensonal hyperplane.
In R<sup>3</sup> a half-space is an 2-dimensional plane, in R<sup>2</sup> a half-space is
a straight line.

A half-space may be described with a linear expression called *half-space expression* of the form

a<sub>1</sub>x<sub>1</sub> + a<sub>2</sub>x<sub>2</sub> + ... + a<sub>n</sub>x<sub>n</sub> + d

with fixed coefficients a<sub>1</sub>,a<sub>2</sub>, ..., a<sub>n</sub>, and d.  A half-space
partitions the euclidian space into 3 distinct and convex areas:

* H<sup>-</sup> or H-: All (x<sub>1</sub>,x<sub>2</sub>, ..., x<sub>n</sub>) where the half-space expression evaluates to a negative value.
* H<sup>0</sup> or H0: All (x<sub>1</sub>,x<sub>2</sub>, ..., x<sub>n</sub>) where the half-space expression evaluates to 0.
* H<sup>+</sup> or H+: All (x<sub>1</sub>,x<sub>2</sub>, ..., x<sub>n</sub>) where the half-space expression evaluates to a positive value.

<center><img src="./images/Halfspace.svg"/></center>

## Convex Polytopes

A convex polytope is the intersection of finitely many halfspaces in R<sup>n</sup>.
In the sequel all polytopes are always convex.

The software in this repository calculates all faces of the polytope from the halfspaces.
For the 3-dimensional space this software creates simple drawings.

<center><img src="./images/Intersect.svg"/></center>

## Data Structure

All vertices, edges, sides and  hyperplanes are called *face*s. Set inclusion of the point sets of the faces defines a
half-order relation on the faces. A half-orders can be represented with a *Hasse diagram* or a *face lattice*. This is
a special form of a *directed acyclic graph*. In Haskell we can use *Map*s to implement directed acyclic graphs.

The next 2 images show all faces of a normal triangle as a geometric object and as a face diagram:

<center><img src="./images/Hasse.svg"/></center>

## Algorithms on Hasse Diagrams

Similar to trees, algorithms can traverse the nodes (aka faces) of a Hasse diagram in preorder or
in postorder method. Different to trees, the traversal method can visit a node only once (*Single visit*) or multiple times (*Multiple visits*).

To specify an algorithm on a Hasse diagram we have to specify to following:

* Traversal method (preorder or postorder)
* Visting frequency (Single or Multiple)
* The processing done on each node / face.

The numbers in the nodes in the following images, represent the visiting sequence.

### Postorder Visiting Sequence (or bottom up)

<center><img src="./images/Postorder.svg"/></center>

### Preorder Visiting Sequence (or top down)

<center><img src="./images/Preorder.svg"/></center>

# HSI Algorithm

The algorithm to calculate a polytope from a set of halfspaces starts with a
huge cube and applies the HSI algorithm for each halfspace in the set.

To visualize the algorihm, we use a simple triangle. The red line is the half-space, the little
arrow points towards H<sup>+</sup>.

<img src="./images/Algo01.svg"/>

The following sctions contain a short description of the algorithm.

## 1. Calculate the Relative Position for each Face

The relative position to a halfspace for a vertex is either:

* **"0"** : if the vertex is part of H<sup>0</sup>
* **"+"** : if the vertex is part of H<sup>+</sup>
* **"-"** : if the vertex is part of H<sup>-</sup>

The relative position of a non-vertex face is the *OR*-combination of all vertices of this face.
eg For an edge with vertices on both sides of H<sup>0</sup> the relative position is **"-+"**.

This is implemented by a postorder walk with single visits to the faces/nodes. For each face, the
following diagram shows the calculated relative position:

<img src="./images/Algo02.svg"/>

## 2. Intersect with H<sup>-</sup>

## 3. Intersect with H<sup>0</sup>

# Higher Dimensions

# Input Program

# Terminology

Terminology is from [2]. We mainly use:

* 0-dim-faces are called `vertices`,
* 1-dim-faces are called `edges`,
* (dim(P) − 1)-faces are called `facets`.
* The dim(P) face is called `polytope`.


### References

[1] [Nef-W. (1978). Beiträge zur Theorie der Polyeder. Bern: Herbert Lang](https://books.google.ch/books/about/Beitr%C3%A4ge_zur_Theorie_der_Polyeder.html?id=3Lm0AAAAIAAJ&redir_esc=y)

[2] [Welz-E, Gärtner-B. (2020). Theory of Combinatorial Algorithms, Chapter 9 ](https://ti.inf.ethz.ch/ew/courses/Geo20/lecture/gca20-9.pdf)

[3] [Article on Convex Polytopes on Wikipedia](https://en.wikipedia.org/wiki/Convex_polytope)