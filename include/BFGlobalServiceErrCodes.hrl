
%% -define( ).

%%           <xsd:enumeration value="OK"/>
%%           <xsd:enumeration value="OK_MESSAGES"/>
%%           <xsd:enumeration value="FAILED_MESSAGE"/>
%%           <xsd:enumeration value="INVALID_USERNAME_OR_PASSWORD"/>
%%           <xsd:enumeration value="USER_NOT_ACCOUNT_OWNER"/>
%%           <xsd:enumeration value="INVALID_VENDOR_SOFTWARE_ID"/>
%%           <xsd:enumeration value="INVALID_PRODUCT"/>
%%           <xsd:enumeration value="INVALID_LOCATION"/>
%%           <xsd:enumeration value="LOGIN_FAILED_ACCOUNT_LOCKED"/>
%%           <xsd:enumeration value="ACCOUNT_SUSPENDED"/>
%%           <xsd:enumeration value="T_AND_C_ACCEPTANCE_REQUIRED"/>
%%           <xsd:enumeration value="POKER_T_AND_C_ACCEPTANCE_REQUIRED"/>
%%           <xsd:enumeration value="LOGIN_REQUIRE_TERMS_AND_CONDITIONS_ACCEPTANCE"/>
%%           <xsd:enumeration value="LOGIN_UNAUTHORIZED"/>
%%           <xsd:enumeration value="ACCOUNT_CLOSED"/>
%%           <xsd:enumeration value="LOGIN_RESTRICTED_LOCATION"/>
%%           <xsd:enumeration value="API_ERROR"/>
-define(LOGIN_ERROR_OK, "OK").

-define(LOGOUT_ERROR_OK, "OK").
-define(LOGOUT_ERROR_API_ERROR, "API_ERROR").
