
-- prepare schems

CREATE EXTENSION "adminpack";
DROP TABLE Assignment;
DROP TABLE Product;
CREATE TABLE Assignment (id SERIAL, product_id int, participant_id int, ts timestamp, action text);
CREATE TABLE Product (id int, current_owner_id int);

-- populate data

INSERT INTO Assignment (product_id, participant_id, ts, action) VALUES (1,2,now(),'c');
INSERT INTO Assignment (product_id, participant_id, ts, action) VALUES (1,3,now(),'m');
INSERT INTO Assignment (product_id, participant_id, ts, action) VALUES (1,4,now(),'m');
INSERT INTO Product (id, current_owner_id) VALUES (1,2);

-- create API functions

CREATE OR REPLACE FUNCTION Chain_of_Custody(id_ int) RETURNS TABLE (_id int, _prod int, _party int, _ts timestamp, _action text) AS $a$
BEGIN return QUERY SELECT id as _id, product_id as _prod, participant_id as _party, ts as _ts, action as _action
                   FROM assignment WHERE product_id = id_ ORDER BY action,ts; END; $a$
LANGUAGE plpgsql;

