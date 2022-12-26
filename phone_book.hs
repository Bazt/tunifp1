data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Ord, Read)

data CountryCode = CountryCode Integer deriving (Eq, Ord)
data PhoneNo =  PhoneNo Integer deriving (Eq, Ord)

instance Show CountryCode where
    show (CountryCode x) = '+':show x

instance Show PhoneNo where
    show (PhoneNo x) = show x
data Phone = Phone  { phoneType :: PhoneType
                    , countryCode :: CountryCode
                    , phoneNo :: PhoneNo
                    } deriving (Eq, Ord)

instance Show Phone where
    show (Phone pt cc pn) = show cc ++ " " ++ show pn ++ " (" ++ show pt ++ ")"

data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving (Eq, Ord, Show)
type PhoneBook = [PhoneBookEntry]

readPhone :: String -> String -> String -> Phone
readPhone t code num = Phone getType getCode getPhoneNumber
    where   getType
                | t == ""                                                            = error "Missing phone type"
                | notElem t ["WorkLandline", "PrivateMobile", "WorkMobile", "Other"] = error "Incorrect phone type"
                | otherwise                                                          = read t :: PhoneType
            getCode
                | code == ""                                = error "Empty country code"
                | any (\c -> notElem c ['0'..'9']) code_f   = error "Incorrect country code"
                | code_i < 0                                = error "Negative country code"
                | notElem code_i predefinedCountryCodes     = error "Unknown country code"
                | otherwise                                 = CountryCode code_i
                    where 
                        code_f = if length code > 2 && take 2 code == "00" then drop 2 code else (if length code > 1 && head code == '+' then tail code else code)
                        code_i = read code_f :: Integer
            getPhoneNumber
                | num == ""                             = error "Empty phone number"
                | any (\c -> notElem c ['0'..'9']) num  = error "Incorrect phone number"
                | otherwise                             = PhoneNo num_i
                    where num_i = read num :: Integer


findEntries :: String -> PhoneBook -> PhoneBook
findEntries n = filter ( (==n) . name )

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry n t code num book 
    | alreadyExists = book
    | otherwise     = PhoneBookEntry n phone_r : book
        where alreadyExists = [] /= [entry | entry <- book, name entry == n && (phoneNo . phone) entry == PhoneNo (read num :: Integer)]
              phone_r       = readPhone t code num

