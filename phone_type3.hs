fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust Nothing"

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Ord, Read)

newtype CountryCode = MakeCountryCode Integer deriving (Eq, Ord)
newtype PhoneNo =  MakePhoneNo Integer deriving (Eq, Ord)

instance Show CountryCode where
    show (MakeCountryCode x) = '+':show x

instance Show PhoneNo where
    show (MakePhoneNo x) = show x
data Phone = Phone  { phoneType :: Maybe PhoneType
                    , countryCode :: Maybe CountryCode
                    , phoneNo :: PhoneNo
                    } deriving (Eq, Ord)



instance Show Phone where
    show (Phone pt cc pn) = let cc_s = if cc /= Nothing then show (fromJust cc) ++ " "  else "" 
                                ph_s = show pn
                                pt_s = if pt /= Nothing then " (" ++ show (fromJust pt) ++ ")" else ""
                            in cc_s ++ ph_s ++ pt_s

data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving (Eq, Ord, Show)
type PhoneBook = [PhoneBookEntry]

toCountryCode :: Integer -> CountryCode
toCountryCode = MakeCountryCode

toPhoneNo :: Integer -> PhoneNo
toPhoneNo = MakePhoneNo

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (MakePhoneNo n) = n

readPhoneType :: String -> Maybe PhoneType
readPhoneType t
    | t == ""                                                            = Nothing
    | notElem t ["WorkLandline", "PrivateMobile", "WorkMobile", "Other"] = Nothing
    | otherwise                                                          = Just (read t :: PhoneType)

readCountryCode :: String -> Maybe CountryCode
readCountryCode code 
    | code == ""                                = Nothing
    | any (\c -> notElem c ['0'..'9']) code_f   = Nothing
    | code_i < 0                                = Nothing
    | notElem code_i predefinedCountryCodes     = Nothing
    | otherwise                                 = Just $ MakeCountryCode code_i
        where 
            code_f = if length code > 2 && take 2 code == "00" then drop 2 code else (if length code > 1 && head code == '+' then tail code else code)
            code_i = read code_f :: Integer

readPhoneNo :: String -> PhoneNo
readPhoneNo num
    | num == ""                             = error "Empty phone number"
    | any (\c -> notElem c ['0'..'9']) num  = error "Incorrect phone number"
    | otherwise                             = MakePhoneNo num_i
        where num_i = read num :: Integer

readPhone :: String -> String -> String -> Phone
readPhone t code num = Phone (readPhoneType t) (readCountryCode code) (readPhoneNo num)

findEntries :: String -> PhoneBook -> PhoneBook
findEntries n = filter ( (==n) . name )

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry n t code num book 
    | alreadyExists = book
    | otherwise     = PhoneBookEntry n phone_r : book
        where alreadyExists = [] /= [entry | entry <- book, name entry == n && (phoneNo . phone) entry == MakePhoneNo (read num :: Integer)]
              phone_r       = readPhone t code num

