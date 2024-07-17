; выдать граф
(defun get_graph () 
    '((IN 1) (IN 3) (1 2) (1 5) (1 3) (5 OUT) (2 4) (3 4) (4 5) (4 6) (6 7) (6 8) (7 8) (7 9) (8 OUT))
)

(defun rev (L res) (cond
    ((null L) res)
    (T (rev (cdr L) (cons (car L) res)))
))

(defun reverse_list (L) (rev L nil))


; преобразовать данные 
(defun add_doors_trans (from to res) (cond
    ((null res) (cons (cons from (cons to nil)) nil))
    ((eq (caar res) from) (cons (cons (caar res) (cons to (cdar res))) (cdr res)))
    (T (cons (car res) (add_doors_trans from to (cdr res))))
))

(defun get_doors_to (L res) (cond
    ((null L) res)
    (T (get_doors_to (cdr L) (add_doors_trans (caar L) (cadar L) (add_doors_trans (cadar L) (caar L) res))))
))
;

; проверить входили мы уже в эту комнату
(defun check_not_in (room visited) (cond
    ((null visited))
    ((eq (car visited) room) nil)
    (T (check_not_in room (cdr visited)))
))

; выдать все комнаты, в которые мы можем попасть из данной
(defun get_rooms (room doors) (cond
    ((null doors) nil) 
    ((eq (caar doors) room) (cdar doors))
    (T (get_rooms room (cdr doors)))
))

; сделать шаг по всем комнатам
(defun add_ways (visited way rooms new_ways) (cond
    ((null rooms) (cons visited (cons new_ways nil)))
    ((check_not_in (car rooms) visited) 
     (add_ways (cons (car rooms) visited)  
               way
               (cdr rooms)
               (cons (cons (car rooms) way) new_ways))
    )
    (T (add_ways visited
                 way
                 (cdr rooms)
                 new_ways)
    )
))

(defun make_a_move (doors visited ways new_ways) (cond
    ((null ways) (cons visited (cons new_ways nil)))
    (T (let ((visited_new_ways (add_ways visited (car ways) (get_rooms (caar ways) doors) new_ways))) 
        (make_a_move doors (car visited_new_ways) (cdr ways) (cadr visited_new_ways))
    ))
))

; проверить пришёл ли алгоритм хотя бы по одному из путей в нужную вернишу
(defun check_end (end ways) (cond
    ((null ways) nil)
    ((eq (caar ways) end) (reverse_list (car ways)))
    (T (check_end end (cdr ways)))
))

(defun BFS (to doors visited ways) (cond
    ((check_end to ways))
    (T (let ((visited_new_ways (make_a_move doors visited ways nil)))
        (BFS to doors (car visited_new_ways) (cadr visited_new_ways))
    ))
))

; основная функция
(defun get_way_from_to (from to graph) (BFS to 
                                    (get_doors_to (get_graph) nil) 
                                    (cons from nil) 
                                    (cons (cons from nil) nil)))
                                
; функция для дополнительной задачи
(defun get_way (graph rooms) (cond
    ((null rooms) nil)
    ((null (cdr rooms)) nil)
    (T (append (get_way_from_to (car rooms) (cadr rooms) graph) (cdr (get_way graph (cdr rooms)))))
))

; примеры
(print (get_way_from_to 'IN 'OUT (get_graph)))
(print (get_way (get_graph) '(IN 6 7 9 OUT)))