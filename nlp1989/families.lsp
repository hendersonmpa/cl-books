;;; families.lsp [Chapter  9] inference rules for family relationships

(defvar infrules)

(setq infrules
 '(((aunt _x _y) (married _x _z) (uncle _z _y))
   ((aunt _x _y) (nephew _y _x) (female _x))
   ((aunt _x _y) (niece _y _x) (female _x))
   ((aunt _x _y) (sibling _x _z) (parent _z _y) (female _x))
   ((brother _x _y) (sibling _x _y) (male _x))
   ((brother_in_law _x _z) (brother _x _y) (married _y _z))
   ((brother_in_law _x _z) (husband _x _y) (sibling _y _z))
   ((child _x _y) (parent _y _x))
   ((child _x _z) (child _x _y) (married _y _z))
   ((child _x _y) (daughter _x _y))
   ((child _x _y) (son _x _y))
   ((cousin _x _y) (cousin _y _x))
   ((cousin _x _y) (parent _z _x) (aunt _z _y))
   ((cousin _x _y) (parent _z _x) (nephew _y _z))
   ((cousin _x _y) (parent _z _x) (niece _y _z))
   ((cousin _x _y) (parent _z _x) (uncle _z _y))
   ((daughter _x _y) (female _x) (child _x _y))
   ((daughter_in_law _x _z) (married _x _y) (son _y _z))
   ((father _x _y) (parent _x _y) (male _x))
   ((father_in_law _x _z) (father _x _y) (married _y _z))
   ((female _x) (aunt _x _y))
   ((female _x) (daughter _x _y))
   ((female _x) (daughter_in_law _x _y))
   ((female _x) (grandmother _x _y))
   ((female _x) (married _x _y) (male _y))
   ((female _x) (mother _x _y))
   ((female _x) (mother_in_law _x _y))
   ((female _x) (niece _x _y))
   ((female _x) (sister _x _y))
   ((female _x) (sister_in_law _x _y))
   ((female _x) (wife _x _y))
   ((grandfather _x _y) (grandparent _x _y) (male _x))
   ((grandmother _x _y) (grandparent _x _y) (female _x))
   ((grandparent _x _z) (parent _x _y) (parent _y _z))
   ((husband _x _y) (wife _y _x))
   ((husband _x _y) (male _x) (married _x _y))
   ((male _x) (brother _x _y))
   ((male _x) (brother_in_law _x _y))
   ((male _x) (father _x _y))
   ((male _x) (father_in_law _x _y))
   ((male _x) (grandfather _x _y))
   ((male _x) (husband _x _y))
   ((male _x) (married _x _y) (female _y))
   ((male _x) (nephew _x _y))
   ((male _x) (son _x _y))
   ((male _x) (son_in_law _x _y))
   ((male _x) (uncle _x _y))
   ((married _x _y) (married _y _x))
   ((married _x _y) (husband _x _y))
   ((married _x _y) (wife _x _y))
   ((mother _x _y) (parent _x _y) (female _x))
   ((mother_in_law _x _z) (mother _x _y) (married _y _z))
   ((nephew _x _y) (aunt _y _x) (male _x))
   ((nephew _x _y) (uncle _y _x) (male _x))
   ((niece _x _y) (aunt _y _x) (female _x))
   ((niece _x _y) (uncle _y _x) (female _x))
   ((parent _x _y) (child _y _x))
   ((parent _x _z) (married _x _y) (parent _y _z))
   ((parent _x _y) (father _x _y))
   ((parent _x _y) (mother _x _y))
   ((parent _x _y) (parent _x _z) (sibling _y _z))
   ((sibling _x _y) (brother _x _y))
   ((sibling _x _y) (parent _z _x) (parent _z _y) (distinct _x _y))
   ((sibling _x _y) (sibling _y _x))
   ((sibling _x _y) (sister _x _y))
   ((sister _x _y) (sibling _x _y) (female _x))
   ((sister_in_law _x _z) (sister _x _y) (married _y _z))
   ((sister_in_law _x _z) (wife _x _y) (sibling _y _z))
   ((son _x _y) (male _x) (child _x _y))
   ((son_in_law _x _z) (married _x _y) (daughter _y _z))
   ((uncle _x _y) (married _x _z) (aunt _z _y))
   ((uncle _x _y) (nephew _y _x) (male _x))
   ((uncle _x _y) (niece _y _x) (male _x))
   ((uncle _x _y) (sibling _x _z) (parent _z _y) (male _x))
   ((wife _x _y) (husband _y _x))
   ((wife _x _y) (female _x) (married _x _y))))
